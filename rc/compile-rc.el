(setq compilation-read-command   nil
      compilation-ask-about-save nil
      compilation-window-height  12
      compilation-error-regexp-systems-list '(gnu)
      compile-command "make ")

;; This gives the compilation buffer its own frame
;;(push "*compilation*" special-display-buffer-names)

(defun my-do-compile (cmd)
  (save-some-buffers (not compilation-ask-about-save) nil)
  (if running-xemacs
      (compile-internal cmd "No more errors")
    (emacs  (compilation-start cmd))))

(defun my-set-compile ()
  (interactive)
  (let ((cmd (read-string "Compile Command: " compile-command)))
    (make-local-variable 'compile-command)
    (setq compile-command cmd)
    (my-do-compile compile-command)))

;;----------------------------------------------------------------
(defvar make-clean-command "make clean all"
  "*Command used by the `make-clean' function.")

(defun make-clean (&optional arg)
  "Run a make clean."
  (interactive "P")
  (if arg
      (setq make-clean-command (read-string "Command: " make-clean-command)))
  (my-do-compile make-clean-command))
