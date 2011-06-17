(add-to-list 'my-compile-dir-list '(".*/LM2.[0-9.]+/") t)

(add-to-list 'my-compile-dir-list '(".*/goldengate/vpn/src/" nil space-indent-4) t)

(add-to-list 'my-compile-dir-list '(".*/goldengate/util/vnodecom/" nil space-indent-4) t)

(defvar sla-dir (expand-file-name "~/goldengate/sla/")
  "* sla directory")

(defun sla-etags (&optional force)
  (interactive)
  ;; SAM If sla-img older than TAGS, drop out?
  ;; SAM How do we decide when to rebuild sla-buf?
  (let ((sla-buf "*sla files*"))
    (when force (kill-buffer sla-buf))
    (when (or (not (bufferp sla-buf)) (eq (buffer-size sla-buf) 0))
      ;; Doing the find in two steps seems to work better
      (call-process "find" nil sla-buf nil sla-dir "-name" "*.c")
      (call-process "find" nil sla-buf nil sla-dir "-name" "*.h"))

    (message "Calling etags...") ; SAM DBG
    (save-current-buffer
      (set-buffer sla-buf)
      (call-process-region (point-min) (point-max)
			   "etags" nil nil nil
			   "-o" (concat sla-dir "TAGS") "-")
      (message "etags done.") ; SAM DBG
      )))

(eval-when-compile (require 'etags))

(defun sla-build (dir arg)
  "sla is weird in that it must be built from the directory *above* sla"

  (tab-indent-2 dir arg)

  (setq buffer-tag-table dir)

  (add-hook 'my-compile-after-hooks 'sla-etags)

  ;; This can't fail....
  (when (string-match "^\\(.*\\)/[^/]+/?$" dir)
    (setq dir (match-string 1 dir)))

  ;; Note that compile-command will already be buffer local
  (setq compile-command
	(concat "make -C " dir " sla"))

  (set (make-local-variable 'make-clean-command)
       (concat "make -C " dir " sla-clean sla")))

(add-to-list 'my-compile-dir-list (list sla-dir nil 'sla-build) t)
