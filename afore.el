(defvar gg-dir (expand-file-name "~/goldengate") "* Base goldengate dir")
(defvar sla-dir nil "* Base sla directory")
(defvar sla-buf "*sla files*" "* SLA file list buffer")
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
  (interactive "P")
  ;; SAM If sla-img older than TAGS, drop out?
  ;; SAM How do we decide when to rebuild sla-buf?
  (when force (kill-buffer sla-buf))
  (my-etag-tree sla-dir sla-buf))

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
