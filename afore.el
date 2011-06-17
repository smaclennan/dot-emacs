(defvar gg-dir (expand-file-name "~/goldengate") "* Base goldengate dir")
(defvar sla-dir nil "* Base sla directory")
(defvar compile-dir-list-was nil)

(eval-when-compile (require 'etags))

(add-to-list 'my-compile-dir-list '(".*/LM2.[0-9.]+/") t)

(defun set-gg-dir (dir)
  (interactive "DDir: ")
  (setq gg-dir dir)
  (setq sla-dir (concat gg-dir "/sla/"))

  ;; Reset my-compile-dir-list
  (if compile-dir-list-was
      (setq my-compile-dir-list compile-dir-list-was)
    (setq compile-dir-list-was my-compile-dir-list))

  (add-to-list 'my-compile-dir-list
	       (list (concat gg-dir "/vpn/src/") nil 'space-indent-4) t)
  (add-to-list 'my-compile-dir-list
	       (list (concat gg-dir "/util/vnodecom/") nil 'space-indent-4) t)
  (add-to-list 'my-compile-dir-list (list sla-dir nil 'sla-build) t)
  )

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

    (save-current-buffer
      (set-buffer sla-buf)
      (call-process-region (point-min) (point-max)
			   "etags" nil nil nil
			   "-o" (concat sla-dir "TAGS") "-")
      )))

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

(set-gg-dir gg-dir)
