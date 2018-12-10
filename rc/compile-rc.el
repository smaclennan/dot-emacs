(setq compilation-read-command   nil
      compilation-ask-about-save nil
      compilation-window-height  16
      compilation-error-regexp-systems-list '(gnu)
      compile-command "make ")

(unless running-xemacs
  ;; Let's see how we like this. Unfortunately it also stops at the
  ;; first warning. Which may be irritating.
  (setq compilation-scroll-output 'first-error))

;; This gives the compilation buffer its own frame
;;(push "*compilation*" special-display-buffer-names)

;;----------------------------------------------------------------
(defun my-do-compile (cmd)
  (save-some-buffers (not compilation-ask-about-save) nil)
  (my-feature-cond
    (xemacs (compile-internal cmd "No more errors"))
    (t (compilation-start cmd))))

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

;;----------------------------------------------------------------
(defun my-compilation-parse (mode)
  "Deal with XEmacs vs GNU Emacs differences in compile"
  (compilation-mode mode)
  (my-feature-cond
   (xemacs
    (goto-char (point-min))
    (compilation-parse-errors nil nil))
   (emacs
    ;; I tried to use compilation but it only worked 90% of the time.
    (setq buffer-read-only nil)
    (compilation--parse-region (point-min) (point-max))
    (setq buffer-read-only t)))
  (goto-char (point-min)))
