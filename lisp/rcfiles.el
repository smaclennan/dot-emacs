;;; rcfiles.el --- Unix-like rc files for Emacs Lisp libraries

;; Copyright (C) 2006, 2007, 2012, 2013 Didier Verna

;; Author: Didier Verna <didier@didierverna.net>
;; Version: 1.0
;; Keywords: extensions, lisp

;; This file is part of el-rcfiles.

;; el-rcfiles is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License version 3, as
;; published by the Free Software Foundation.

;; el-rcfiles is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.


;;; Commentary:

;; The purpose of el-rcfiles is to provide the equivalent of traditional
;; Unix rc files (i.e. configuration files) for Emacs Lisp
;; libraries. The advantages of using configuration files are the
;; following:
;;   - your initialization file is less bloated,
;;   - since configuration files are lazily loaded, your Emacs session
;;     is (or begins) lighter. That is unless you already use lots of
;;     EVAL-AFTER-LOAD forms...

;; Usage:

;; 1. Load the library, go to the rcfiles Custom group and tweak (or not).
;; 2. Put a call to (rcfiles-register-rc-files) in your initialization
;;    file. This function can also be called interactively anytime you
;;    add, remove or modify a configuration file.
;; 3. Put your configuration code for a library `foo' in a file called
;;    `<rcfiles-directory>/foo<rcfiles-pseudo-extension>.el'.


;;; Code:

(defvar rcfiles-version "1.0"
  "Current version number of el-rcfiles.")

(defun rcfiles-version ()
  "Show the current version number of el-rcfiles."
  (interactive)
  (message "%s" rcfiles-version))


(defgroup rcfiles nil
  "Configuration files for Emacs Lisp libraries."
  :group 'emacs)

(defcustom rcfiles-directory
  (if (featurep 'sxemacs) "~/.sxemacs/rc"
    (if (featurep 'xemacs) "~/.xemacs/rc"
      "~/.emacs.d/rc"))
  "Directory where el-rcfiles looks for configuration files."
  :group 'rcfiles
  :type 'string)

(defcustom rcfiles-pseudo-extension "-rc"
  "Pseudo extension for configuration files.
It is added between the name of the library and the .el
extension."
  :group 'rcfiles
  :type 'string)

(if (>= emacs-major-version 25)
    (require 'cl-seq) ;; cl-remove-if
  (require 'cl))

(defun rcfiles-rc-files ()
  "Return the list of configuration files currently available.
File names are expanded but their extension is removed."
  (let ((ext-regexp
	 ;; Emacs change... was .el[c]?
	 ;; The comment below about dups is false for Emacs. Since I
	 ;; never have just raw .elc files we only look for .el
	 (concat (regexp-quote rcfiles-pseudo-extension) "\\.el$")))
    (mapcar #'file-name-sans-extension
	    ;; #### NOTE: potential duplicates (such as when there is
	    ;; both a .el and a .elc file) are not a problem because
	    ;; EVAL-AFTER-LOAD takes care of that.
	    (directory-files rcfiles-directory t ext-regexp))))

(defun rcfiles-prune (rcfiles)
  "Unregister configuration files not in RCFILES."
  (let ((dir
	 (concat "^" (regexp-quote (expand-file-name rcfiles-directory)))))
    (setq after-load-alist
	  (cl-remove-if (lambda (file) ;; Emacs remove-if -> cl-remove-if
		       (and (stringp file)
			    (string-match dir file)
			    (not (member file rcfiles))))
		     after-load-alist
		     :key (lambda (form)
			    (ignore-errors
			      (let ((load-form
				     ;; are we having fun yet?
				     (cl-caddr (cadr (cadr (cadr form))))))
				(when (eq (car load-form) 'rcfiles-try-load)
				  (cadr load-form)))))))))

(defun rcfiles-try-load (rcfile)
  "Attempt to load RCFILE. If loading fails, throw a warning."
  (condition-case ignore (load rcfile)
    (error (warn (format "Unable to load %s.
Maybe you need to call RCFILES-REGISTER-RC-FILES again?"
			 rcfile)))))

(defun rcfiles-register (rcfiles)
  "Register the configuration files in the RCFILES list."
  (let ((len (length rcfiles-pseudo-extension)))
    (dolist (rcfile rcfiles)
      (eval-after-load (file-name-nondirectory (substring rcfile 0 (- len)))
	`(rcfiles-try-load ,rcfile)))))

;;;###autoload
(defun rcfiles-register-rc-files ()
  "Register the configuration files currently available.
This function can be called at startup and every time the
registration changes. Obsolete entries are removed and new ones
are added."
  (interactive)
  (let ((rcfiles (rcfiles-rc-files)))
    (rcfiles-prune rcfiles)
    (rcfiles-register rcfiles)))


(provide 'rcfiles)

;;; rcfiles.el ends here
