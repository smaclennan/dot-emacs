;;; my-cscope.el --- Yet another cscope interface... but it is mine.
;; Copyright (C) 2017-2018 Sean MacLennan
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

;; For the menu do something like:
;; (add-hook 'c-mode-hook 'my-cscope-setup)

(defgroup my-cscope nil "cscope variables" :group 'tools)

(defcustom my-cscope-prog "cscope" "* The cscope program." :type 'string)

;; Note: -R has no extra overhead if you have no subdirs
(defcustom my-cscope-args "-R" "* Extra cscope arguments." :type 'string)

(defcustom my-cscope-dir nil
  "* Directory to start looking for cscope files in." :type 'string)

(defvar mcs-regexp "^\\([^ ]+\\) \\([^ ]+\\) \\([0-9]+\\)"
  "* Regular expression to match filename and line.")

(require 'cl)
(require 'compile)
(require 'etags)

(defmacro mcs-feature-cond (&rest clauses)
  "Test CLAUSES for feature or function at compile time.
Each clause is (FEATURE BODY...)."
  (dolist (x clauses)
    (when (or (featurep (car x)) (fboundp (car x)))
      (return (cons 'progn (cdr x))))))

(defun mcs-dir ()
  "Find the cscope.out file directory. Use `my-cscope-dir' if
set, else start looking at `default-directory'."
  (let ((dir (if my-cscope-dir my-cscope-dir default-directory)))
    ;; Sanitize the directory
    (setq dir (expand-file-name (file-name-as-directory dir)))
    (catch 'found
      (while (not (equal dir "/"))
	(when (file-exists-p (concat dir "cscope.out"))
	  (throw 'found dir))
	;; This removes the last directory
	(setq dir (file-name-directory (directory-file-name dir))))
      (error "No cscope.out file found."))))

(defun mcs-parse-output (mode)
  "Deal with XEmacs vs GNU Emacs differences in compile"
  (compilation-mode mode)
  (mcs-feature-cond
   (xemacs
    (goto-char (point-min))
    (compilation-parse-errors nil nil))
   (emacs
    ;; I tried to use compilation but it only worked 90% of the time.
    (setq buffer-read-only nil)
    (compilation--parse-region (point-min) (point-max))
    (setq buffer-read-only t)))
  (goto-char (point-min)))

(defun mcs-push-tag-mark ()
  "Deal with, mainly GNU Emacs, push-tag-mark differences"
  (mcs-feature-cond
   (xemacs (push-tag-mark))
   (xref-push-marker-stack (xref-push-marker-stack))
   (emacs (ring-insert find-tag-marker-ring (point-marker)))))

(defconst mcs-prompts
  '("0 Find this C symbol"
    "1 Find this function definition"
    "2 Find functions called by this function"
    "3 Find functions calling this function"
    "4 Find this text string"
    "5 Change this text string"
    "6 Find this egrep pattern"
    "7 Find this file"
    "8 Find files #including this file"
    "9 Find assignments to this symbol")
  "The prompts for the different cscope commands. This must match
the order in cscope.")

(defun my-cscope-to-buffer (sym type)
  "Low level function to call cscope of TYPE (0-9 inclusive) on
SYM into current buffer."
  (let ((default-directory (mcs-dir))
	(cmd (concat my-cscope-prog " " my-cscope-args " -L -"
		     (number-to-string type) sym)))
    (insert cmd "\n\n") ;; XEmacs needs the blank line
    (call-process-shell-command cmd nil t)))

;;;###autoload
(defun my-cscope (type &optional sym)
  "Call cscope on SYM of type TYPE. Set a prefix arg for TYPE. TYPE
must be 0 to 9 inclusive (see man cscope).

SYM defaults to the current word at the point.

If there is only one result, then `my-cscope' will just goto the
symbol. If there are multiple results, you can select the result
or use `next-error' to go through the results.

The cscope command run is:
`my-cscope-prog' `my-cscope-args' -L -<TYPE><SYM>."
  (interactive "p")
  (when (or (< type 0) (> type 9))
    (error "invalid type %d: must be 0 to 9 inclusive." type))

  (unless sym
    (let ((word (current-word))
	  (prompt (nth type mcs-prompts)))
      (setq sym (read-string (concat prompt " [" word "]: ") nil nil word))))

  (let ((count 0) tagname)
    (with-current-buffer (get-buffer-create "*cscope*")
      (setq default-directory (mcs-dir)) ;; must be setq
      (setq tagname (concat default-directory "cscope.tags"))
      (setq buffer-read-only nil)
      (erase-buffer)

      (my-cscope-to-buffer sym type)
      ;; Fixup the buffer to look how compilation wants it
      (goto-char (point-min))
      (while (re-search-forward mcs-regexp nil t)
	(setq count (1+ count))
	(replace-match (concat (match-string 1) ":" (match-string 3) ":1 " (match-string 2))))

      (mcs-parse-output "cscope"))

    (mcs-push-tag-mark)
    (display-buffer "*cscope*" '(nil (window-height . 16)))
    (when (eq count 1) (first-error))))

;;;###autoload
(defun my-cscope-at-point (type)
  "Grab word at point and call `my-cscope'."
  (interactive "p")
  (my-cscope type (current-word)))

;;;###autoload
(defun my-cscope-setup ()
  "Initialize the cscope menu."
  (unless (boundp 'my-cscope-menu)
    (let ((menu '("Cscope...")))
      (dolist (i (number-sequence 0 9))
	(nconc menu (list (vector (nth i mcs-prompts) (list 'my-cscope i) t))))
      (easy-menu-define my-cscope-menu nil "My Cscope Menu" menu)
      (easy-menu-add-item nil '("C") my-cscope-menu ""))))

;;; --- cscope on /usr/include

(defvar my-cscope-include-dir (expand-file-name "~/.cscope-include")
  "*Where to store the /usr/include cscope output files. Must be the full path.")

;;;###autoload
(defun my-cscope-include-update ()
  "Update the cscope files in `my-cscope-include-dir'. This can
take a long time the first time it is run."
  (interactive)
  (unless (file-directory-p my-cscope-include-dir)
    (make-directory my-cscope-include-dir t))
  (let ((cmd (concat "cscope -b -R -s/usr/include -f"
		     my-cscope-include-dir "/cscope.out"))
	rc)
    (message "Please wait for %s..." cmd)
    (unless (eq (call-process-shell-command cmd) 0)
      (error "Unable to update cscope.out"))))

;;;###autoload
(defun my-cscope-include (type &optional sym)
  (interactive "P")
  (unless type (setq type 0)) ;; type 0 makes more sense for includes
  (let ((my-cscope-dir my-cscope-include-dir))
    (unless (file-exists-p (concat my-cscope-include-dir "/cscope.out"))
      (my-cscope-include-update))
    (my-cscope type sym)))

;;;###autoload
(defun my-cscope-include-at-point (type)
  (interactive "P")
  (my-cscope-include type (current-word)))

(global-set-key "\C-ci" 'my-cscope-include-at-point)


(provide 'my-cscope)
