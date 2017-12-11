
;; Everybody uses mouse-1... except xcscope
(define-key cscope-list-entry-keymap [mouse-1]
  'cscope-mouse-select-entry-other-window)

;; I like speed
(setq cscope-command-args "-q")

;; ----- cscope-update

(defvar cscope-update-args "-q -R -b"
  "*Args to use when calling updating cscope.")

(defvar cscope-update-hooks nil
  "*Optional hook function(s) to call before calling
cscope. Handy for updating a file list for example. It is called
from the directory where the cscope files reside.")

(defun my-cscope-dir ()
  (unless (and (boundp 'cscope-minor-mode) cscope-minor-mode)
    (error "Cscope not enabled in buffer"))
  (cscope-search-directory-hierarchy
   (cscope-canonicalize-directory cscope-initial-directory)))

(defun cscope-update (arg)
  "Update the cscope files. If ARG is non-nil, run the
`cscope-update-hooks' before calling cscope.

Note: `cscope-update' tries to find the cscope files, but you may
need to help it out by setting `cscope-initial-directory'."
  (interactive "P")
  (let ((default-directory (my-cscope-dir)))
    (when arg (run-hooks 'cscope-update-hooks))
    (shell-command (concat cscope-program " " cscope-update-args))))
