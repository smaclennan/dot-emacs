;;; my-c-tools.el - Various C programming tools/utilities.
;; Copyright (C) 2010-2012 Sean MacLennan

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;;; ---- Sparse
;; Call sparse on the current buffer with output to the compilation
;; buffer.
;;
;; You can get sparse here:
;; https://sparse.wiki.kernel.org/index.php/Main_Page

(defvar my-sparse-prog "sparse"
  "*The sparse executable.")

(defvar my-sparse-args (if (eq system-type 'gnu/linux) "-D__linux__" nil)
  "*Args to pass to sparse")

(require 'my-compile)
(require 'cc-mode)

(defun my-do-compile (cmd)
  (save-some-buffers (not compilation-ask-about-save) nil)
  (compilation-start cmd))

;; Basically stolen from XEmacs emacs-version>=
(defun my-kernel>= (major minor &optional patch)
  "Return true if the `my-kernel-vers' is >= to the given MAJOR, MINOR,
   and PATCH numbers.
The MAJOR and MINOR version numbers are required, but the PATCH is optional."
  (let ((vers (split-string my-kernel-vers "\\.")))
    (cond ((> (nth 0 vers)  major))
	  ((< (nth 0 vers) major) nil)
	  ((> (nth 1 vers) minor))
	  ((< (nth 1 vers) minor) nil)
	  ((null patch))
	  ((>= (nth 2 vers) patch)))))

;;;###autoload
(defun my-sparse (&optional user-args)
  "Run sparse against the current buffer. Output goes to the
compilation buffer so that `next-error' will work."
  (interactive)
  (my-do-compile (concat my-sparse-prog " " my-sparse-args " "
			 user-args " " (buffer-file-name))))

;;;###autoload
(defun my-sparse-linux (&optional user-args)
  "Run sparse against the current buffer using Linux kernel
args. Output goes to the compilation buffer so that `next-error' will
work."
  (interactive)
  (let ((cmd (concat "make C=2 " user-args)))
    (my-do-compile cmd)))

;;;; ---- Checkpatch

(defvar my-checkpatch-prog (concat my-kernel-dir "/scripts/checkpatch.pl")
  "* The checkpatch program.")

(defvar my-checkpatch-args "--emacs --file --no-tree"
  "* Args to pass to checkpatch. Note that --no-color is added if
  needed")

;;;###autoload
(defun my-checkpatch ()
  "Run checkpatch against the current buffer. Output goes to the
compilation buffer so that `next-error' will work."
  (interactive)

  (save-some-buffers (not compilation-ask-about-save) nil)

  (add-to-list 'compilation-finish-functions 'my-checkpatch-cleanup)

  ;; We cannot call `compile' here since it sets the compile command
  (let (cmd (args my-checkpatch-args))
    ;; If the kernel is >= 4.2 we must add --no-color
    (when (my-kernel>= 4 2)
      (setq args (concat args " --no-color")))
    (setq cmd (format "%s %s %s" my-checkpatch-prog args (buffer-file-name)))
    (my-do-compile cmd)))

(defun my-checkpatch-cleanup (buf status)
  "Massage the checkpatch compilation buffer. This removes a final
false match."
  (save-current-buffer
    (set-buffer buf)
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "^total:" nil t)
	(replace-match "total"))

      ;; Emacs also gets the #<lineno>: FILE: <file> lines wrong
      (goto-char (point-min))
      (while (re-search-forward "^#[0-9]+: FILE: .*$" nil t)
	(replace-match ""))
      ))

  (setq compilation-finish-functions
	(delete 'my-checkpatch-cleanup compilation-finish-functions)))

;;;###autoload
(defun lsmod-diff (file)
  "Diff a file against the current modules."
  (interactive "fFile: ")
  (let ((buf-was (get-buffer-create "*lsmod was*"))
	(buf-is (get-buffer-create "*lsmod is*")))
    (shell-command (format "sort %s | cut -d' ' -f1" file) buf-was buf-was)
    (shell-command "lsmod | sort | cut -d' ' -f1" buf-is buf-is)
    (ediff-buffers buf-is buf-was)))

(defun gcov-100 ()
  (with-current-buffer (get-buffer-create "*gcov*")
    (goto-char (point-min))
    (catch 'done
      (unless (looking-at "Function .*") (throw 'done nil))
      (goto-char (match-end 0)) (forward-char)
      (unless (looking-at "Lines executed:100\.00%.*") (throw 'done nil))
      (goto-char (match-end 0)) (forward-char)
      (unless (looking-at "No branches")
	(unless (looking-at "Branches executed:100\.00%.*") (throw 'done nil))
	(goto-char (match-end 0)) (forward-char)
	(unless (looking-at "Taken at least once:100\.00%.*") (throw 'done nil)))
      (goto-char (match-end 0)) (forward-char)
      (or (looking-at "Calls executed:100\.00%.*")
	  (looking-at "No calls")
	  (throw 'done nil))
      (throw 'done t))))

;;;###autoload
(defun gcov-func (func &optional file)
  "Given a FUNC, run gcov on the current buffer and find the
function in the output. If FILE is specified, use that file
rather than the current buffer."
  (interactive "sFunc: ")
  (unless file (setq file (buffer-file-name)))
  (let ((dir (file-name-directory file)))
    (unless dir (setq dir default-directory))
    (with-current-buffer (get-buffer-create "*gcov*")
      (setq default-directory dir)
      (erase-buffer)
      (call-process "gcov" nil t nil "-fb" file)
      (goto-char (point-min))
      (re-search-forward (concat "^Function '" func "'$"))
      (delete-region (point-min) (match-beginning 0))
      ;; We will never be at EOB because the files come after functions
      (re-search-forward "^$")
      (delete-region (point) (point-max))
      (goto-char (point-min))
      )
    (if (gcov-100)
	(message "You da man!")
      (display-buffer "*gcov*"))))

;;;###autoload
(defun set-c-vars (tabs width)
  "Set C tab mode. With a prefix-arg set tab width."
  (interactive (list
		(yes-or-no-p "Tabs? ")
		(if current-prefix-arg
		    (read-number "Width: " c-basic-offset)
		  4)))
  (setq indent-tabs-mode tabs
	c-basic-offset width
	tab-width width))

(provide 'my-c-tools)
