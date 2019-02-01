;;; my-compile.el - compile command helpers

;; Copyright (C) 1996-2018 Sean MacLennan

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License version
;; 2 as published by the Free Software Foundation.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABIL`ITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

; The `my-compile-command' function was written to handle code bases
; with sub-directories where you have to build from an upper level, so
; `M-x compile' just won't work.  By setting up the
; `my-compile-dir-list', `my-compile-command' allows you to just hit
; `M-x compile' anywhere in the code base and it just works (tm).
;
; It can do much more that that though. Since it can call a function
; based on the directory you can do almost anything. Setting buffer
; local variables is an obvious one.
;
; The `my-compile-command' function is meant to be called from the
; `c-mode-common-hook'. It takes the current buffers directory and matches
; it against the `my-compile-dir-list'. If there is a match, it sets the
; `compile-command' appropriately.
;
; The help for `my-compile-command' describes the elements of the list.

(eval-when-compile (require 'cpuinfo))
(require 'compile)

;; The 'car split-string' is a portable way to remove the trailing NL
(defvar my-kernel-vers (car (split-string (shell-command-to-string "uname -r")))
  "* Current kernel version.")

(defvar my-arch (car (split-string (shell-command-to-string "uname -m")))
  "* Current architecture.")

(defvar my-kernel-dir (file-chase-links (concat "/lib/modules/" my-kernel-vers "/build"))
  "* Current kernel directory.")

(defvar my-compile-check-svn nil
  "* If non-nil, check for svn directories.")

(defun my-kernel-vers (&optional kvers)
  "Splits KVERS (defaults to `my-kernel-vers') up into a list
of (MAJOR MINOR PATCH BUILD RC EXTRA). All are numbers except
EXTRA; which is a possibly empty string."
  (unless kvers (setq kvers my-kernel-vers))
  (let* ((buildfile (concat "/lib/modules/" kvers "/build/.version"))
	 (build
	  (if (file-exists-p buildfile)
	      (string-to-number (shell-command-to-string (concat "cat " buildfile)))
	    0))
	 vers extra)
    (if (string-match "\\([0-9]+\\).\\([0-9]+\\).\\([0-9]+\\)\\(.*\\)" kvers)
	(setq vers (list (string-to-number (match-string 1 kvers))
			(string-to-number (match-string 2 kvers))
			(string-to-number (match-string 3 kvers))
			build)
	      extra (match-string 4 kvers))
      (setq vers (list 0 0 0 build) extra ""))
    (if (string-match "-rc\\([0-9]+\\)\\(.*\\)" extra)
	(setq vers (append vers (list (string-to-number (match-string 1 extra))
				      (match-string 2 extra))))
      (setq vers (append vers (list 0 extra))))
    vers))

(defun my-kernel-vers-real (&optional kvers)
  "Like `my-kernel-vers' except it looks up the actual version
associated with the kernel directory. Also adds a MATCH which is
true if KVERS matches the version in the kernel directory."
  (unless kvers (setq kvers my-kernel-vers))
  (let ((relfile (concat "/lib/modules/" kvers "/build/include/config/kernel.release"))
	kstr vers)
    (unless (file-exists-p relfile) (error "%s not found" relfile))
    (setq kstr (shell-command-to-string (concat "cat " relfile)))
    (setq kstr (car (split-string kstr))) ;; remove NL
    (setq vers (my-kernel-vers kstr))
    (setq vers (append vers (list (string= kvers kstr))))
    vers))

;; Basically stolen from XEmacs emacs-version>=
(defun my-kernel>= (major minor &optional patch)
  "Return true if the `my-kernel-vers' is >= to the given MAJOR, MINOR,
   and PATCH numbers.
The MAJOR and MINOR version numbers are required, but the PATCH is optional."
  (let ((vers (my-kernel-vers)))
    (cond ((> (nth 0 vers)  major))
	  ((< (nth 0 vers) major) nil)
	  ((> (nth 1 vers) minor))
	  ((< (nth 1 vers) minor) nil)
	  ((null patch))
	  ((>= (nth 2 vers) patch)))))

(defun my-kernel-set-dir (dir)
  "Set `my-kernel-dir' verifying that the directory exists and following all links."
  (interactive "DKernel dir: ")
  (setq dir (file-chase-links (expand-file-name dir)))
  (unless (file-exists-p dir)
      (unless (y-or-n-p (concat dir " does not exist... set anyway? "))
	(error "Not set.")))
  (setq my-kernel-dir dir)
  (message "Kernel = %s" my-kernel-dir))

(defun my-kernel-set-version (version)
  "Set the `my-kernel-vers' and `my-kernel-dir' variables with validation."
  (interactive "sVersion: ")
  (my-kernel-set-dir (concat "/lib/modules/" version "/build"))
  (setq my-kernel-vers version))

(defvar make-j (if (fboundp 'cpuinfo-num-processors)
		   (let ((ncpus (cpuinfo-num-processors)))
		     (if (< ncpus 8)
			 (format "-j%d" (* ncpus 2))
		       (format "-j%d" ncpus)))
		 "-j2") ;; reasonable default
  "* -Jn value to pass to makes.")

(defvar my-compile-dir-list
  (list
   (list (concat "^" my-kernel-dir "/") make-j "linux")
   (list "^/usr/src/[^/]*linux[^/]*/" make-j "linux")
   ;; emacs needs gnu
   (list ".*/s?x?emacs[^/]*/src/" make-j "gnu")
   (list ".*/s?x?emacs[^/]*/" make-j "gnu")
   ;; Busybox
   (list ".*/busybox/" make-j)
   )
  "*A list of directory matches used by `my-compile-command' to set
the compile command.

Each match is a list, only the first element is required:

  * The first element is a regexp for the directory.
  * The second element is an arg string to pass to make.
  * The third element is either a string which defines the style to
    use, or a lisp function to call. The lisp function will be passed
    the directory matched and the target as parameters.

If `my-compile-check-svn' is non-nil, the first element can also be svn://<url>.
Only the first match is used so order is important.")

;; Needed for Emacs.
(eval-when-compile
  (require 'svn)
  (require 'cc-vars))

(defun my-compile-command ()
  "Set the compile command for the current file.
Go through the 'my-compile-dir-list' looking for a match.
If we match, the second element is an optional target and the
third argument is an optional function to call. The optional
function will be called after the compile command is set."
  (interactive)
  (let ((svn-dir (my-compile-svn-url))
	match dir arg func-or-style matched)

    (cl-loop for list in my-compile-dir-list until dir do
      (setq match (car list))
      (if (string-match match default-directory)
	  (progn
	    (setq dir (match-string 0 default-directory))
	    (setq matched list))
	(when (and svn-dir (string-match match svn-dir))
	  ;; The difference in length between svn-dir and the match
	  ;; string is the same as the difference in length between
	  ;; default-directory and the directory we want.
	  (setq dir (substring default-directory 0
			       (- (match-end 0) (length svn-dir) 1)))
	  (setq matched list))))

    (when dir
      (setq arg (nth 1 matched)
	    func-or-style (nth 2 matched))
      (when (or arg (not (equal dir default-directory)))
	(set (make-local-variable 'compile-command)
	     (concat "make -C " dir " " arg)))
      (cond
       ((stringp func-or-style)
	(when c-buffer-is-cc-mode ;; Make sure c-ish code, not Makefile
	  ;; I set tab-width in my-c-mode-common-hook. Reset it here.
	  (kill-local-variable 'tab-width)
	  (c-set-style func-or-style)))
       ((fboundp func-or-style)
	(funcall func-or-style dir arg))))))

;; Make sure we are *after* my-c-mode-common-hook
(add-hook 'c-mode-common-hook 'my-compile-command t)
(add-hook 'makefile-mode-hook 'my-compile-command t)

(defun my-compile-svn-url ()
  "Perform an svn info on the current buffer to get the svn url.
Output will be nil if this directory not in svn."
  ;; Note: we must save dir before the set-buffer
  (if my-compile-check-svn
      (let (url (dir default-directory))
	(save-current-buffer
	  (set-buffer (get-buffer-create "*svn output*"))
	  (erase-buffer)
	  (when (eq 0 (call-process "svn" nil t nil "info" dir))
	    (goto-char (point-min))
	    (when (re-search-forward "^URL: \\(.+\\)" nil t)
	      (setq url (concat (match-string 1) "/"))))
	  url))
    nil))

;; Some helpers

(defun indent-N (n tab-mode)
  "Helper function for the (space|tab)-indent-N helpers ;)"
  (setq indent-tabs-mode tab-mode)
  (setq c-basic-offset n)
  (setq tab-width n))

(defun space-indent-4 (dir arg) (indent-N 4 nil))

(defun tab-indent-4 (dir arg) (indent-N 4 t))

(defun space-indent-2 (dir arg) (indent-N 2 nil))

(defun tab-indent-2 (dir arg) (indent-N 2 t))

;; Init hooks
(defvar my-compile-init-hooks nil
  "* Hook(s) to be run at my-compile init.")

(run-hooks 'my-compile-init-hooks)

;; Add hooks before and after compile

(defvar my-compile-before-hooks nil
  "* Hook(s) to be run before compile command is executed.")

(defvar my-compile-after-hooks nil
  "* Hook(s) to be run after compile command is executed.")

(defadvice compile (around my-compile-run-hooks activate)
  (run-hooks 'my-compile-before-hooks)
  ad-do-it
  (run-hooks 'my-compile-after-hooks))

(provide 'my-compile)
