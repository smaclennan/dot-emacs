(add-to-list 'load-path (concat user-emacs-directory "compat"))
(load "compat-loaddefs" t)

;; Back before GNU Emacs went insane about common lisp
(require 'cl)

(defalias 'cl-remove-if 'remove-if)
(defalias 'cl-loop 'loop)
(defalias 'cl-block 'block)
(defalias 'cl-return 'return)
(defalias 'cl-caddr 'caddr)
(defalias 'cl-cdadr 'cdadr)
(defalias 'cl-cadadr 'cadadr)

;; With an old Emacs you probably have an old kernel
(when (eq system-type 'gnu/linux)
  (eval-after-load "cpuinfo"
    `(defun sys-nproc ()
       (let ((procs 0))
	 (with-temp-buffer
	   ;; insert-file-contents does not work on /proc
	   (call-process "cat" nil t nil "/proc/cpuinfo")
	   (goto-char (point-min))
	   (while (re-search-forward "^processor" nil t)
	     (setq procs (1+ procs))))
	 procs))))
 
