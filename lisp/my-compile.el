;;; my-compile.el - compile command helper

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
; The help for `my-compile-dir-list' describes the elements of the list.

(require 'sam-common) ;; uname()
(eval-when-compile (require 'cc-vars))

(defvar my-kernel-vers (uname "-r")
  "* Current kernel version.")

(defvar my-kernel-dir (file-chase-links (concat "/lib/modules/" my-kernel-vers "/build"))
  "* Current kernel directory.")

(defvar make-j "-j8"
  "* -Jn value to pass to makes.")

(defun gdb-func (matched-dir target)
  (setq compile-command (concat "make " make-j " -C " matched-dir "/build/objdir"))
  (c-set-style "gnu")
  (setq c-basic-offset 2)
  (setq tab-width 8))

(defvar my-compile-dir-list
  (list
   (list (concat "^" my-kernel-dir "/") make-j "linux")
   (list "^/usr/src/[^/]*linux[^/]*/" make-j "linux")
   ;; emacs needs gnu
   (list ".*/s?x?emacs[^/]*/src/" make-j "gnu")
   (list ".*/s?x?emacs[^/]*/" make-j "gnu")
   ;; Busybox
   (list ".*/busybox/" make-j)
   ;; gdb
   (list "^.*/gdb-[0-9.]+/" nil 'gdb-func)
   )
  "*A list of directory matches used by `my-compile-command' to set
the compile command.

Each match is a list, only the first element is required:

  * The first element is a regexp for the directory.
  * The second element is an arg string to pass to make.
  * The third element is either a string which defines the style to
    use, or a lisp function to call. The lisp function will be passed
    the directory matched and the target as parameters.

Only the first match is used so order is important.")

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

(defun my-compile-command ()
  "Set the compile command for the current file.
Go through the 'my-compile-dir-list' looking for a match."
  (interactive)
  (let (dir arg func-or-style matched)
    (cl-loop for list in my-compile-dir-list until dir do
      (when (string-match (car list) default-directory)
	(setq dir (match-string 0 default-directory))
	(setq matched list)))

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

;;;###autoload
(defun my-compile-delete (regexp)
  "Helper function to delete an entry from the `my-compile-dir-list'.
Asks for the directory regular expression."
  (interactive "sRegexp: ")
  (let ((entry (assoc regexp my-compile-dir-list)))
    (if entry
	(setq my-compile-dir-list (delq entry my-compile-dir-list))
      (error "%s not found" regexp))))

(provide 'my-compile)
