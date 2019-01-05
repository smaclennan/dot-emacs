(setq compilation-read-command   nil
      compilation-ask-about-save nil
      compilation-window-height  16
      compilation-error-regexp-systems-list '(gnu)
      compile-command "make ")

;; Let's see how we like this. Unfortunately it also stops at the
;; first warning. Which may be irritating.
(setq compilation-scroll-output 'first-error)

;; This gives the compilation buffer its own frame
;;(push "*compilation*" special-display-buffer-names)

;;----------------------------------------------------------------
(defun my-do-compile (cmd)
  (save-some-buffers (not compilation-ask-about-save) nil)
  (compilation-start cmd))

(defun my-set-compile ()
  (interactive)
  (let ((cmd (read-string "Compile Command: " compile-command)))
    (make-local-variable 'compile-command)
    (setq compile-command cmd)
    (my-do-compile compile-command)))

(defvar make-clean-command "make clean all"
  "*Command used by the `make-clean' function.")

(defun make-clean (&optional arg)
  "Run a make clean."
  (interactive "P")
  (if arg
      (setq make-clean-command (read-string "Command: " make-clean-command)))
  (my-do-compile make-clean-command))
