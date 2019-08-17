(setq compilation-read-command   nil
      compilation-ask-about-save nil
      compilation-window-height  16)

;; If a file was opened from the command line, any make command was
;; overwritten so use setq-default.
(setq-default compile-command "make ")

;; Let's see how we like this. Unfortunately it also stops at the
;; first warning. Which may be irritating.
(setq compilation-scroll-output 'first-error)

;; This gives the compilation buffer its own frame
;;(push "*compilation*" special-display-buffer-names)

;;----------------------------------------------------------------
(defun my-set-compile ()
  (interactive)
  (let ((cmd (read-string "Compile Command: " compile-command)))
    (set (make-local-variable 'compile-command) cmd)
    (save-some-buffers (not compilation-ask-about-save) nil)
    (compilation-start cmd)))

(defvar make-clean-command "make clean all"
  "*Command used by the `my-make-clean' function.")

(defun my-make-clean (&optional arg)
  "Run a make clean."
  (interactive "P")
  (if arg
      (setq make-clean-command (read-string "Command: " make-clean-command)))
  (save-some-buffers (not compilation-ask-about-save) nil)
  (compilation-start make-clean-command))
