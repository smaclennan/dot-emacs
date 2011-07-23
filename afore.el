(defvar sla-buf "*sla files*" "* SLA file list buffer")

(defvar gg-svn-url "svn://localhost/gg") ;; Sandbox
;;(defvar gg-svn-url "svn://redbox/repo/projects/goldengate")

(add-to-list 'my-compile-dir-list '(".*/LM2.[0-9.]+/") t)

;; throughput engine must be before sla proper
(add-to-list 'my-compile-dir-list
	     (list (concat gg-svn-url "/.*/sla/throughput-engine/")
		   "throughputengine" 'sla-build) t)
(add-to-list 'my-compile-dir-list
	     (list (concat gg-svn-url "/.*/sla/")
		   "sla" 'sla-build) t)

;; Sighhh.... vpn source uses 2 and 3 and sometimes 4
(add-to-list 'my-compile-dir-list
	     (list (concat gg-svn-url "/.*/vpn/src/") nil 'space-indent-2) t)

(add-to-list 'my-compile-dir-list
	     (list (concat gg-svn-url "/.*/util/vnodecom/") nil 'space-indent-4) t)

;; *****
;; etags and compile support

(eval-when-compile (require 'etags))

(defvar sla-dir nil "SLA directory. Set in `sla-build'.")
(make-variable-buffer-local 'sla-dir)

(defun sla-etags (&optional force)
  (interactive "P")
  (unless sla-dir (error "No SLA directory associated with this buffer."))
  ;; SAM If sla-img older than TAGS, drop out?
  ;; SAM How do we decide when to rebuild sla-buf?
  (when force (kill-buffer sla-buf))
  (my-etag-tree sla-dir sla-buf))

(defun sla-build (dir arg)
  "This should be called via `my-compile-dir-list'."

  ;; Correct for throughputengine directory
  (when (string= arg "throughputengine")
      (setq dir (expand-file-name (concat dir "/.."))))

  (tab-indent-2 dir arg)

  ;; etags
  (setq sla-dir (concat dir "/"))
  (setq buffer-tag-table dir)
  (add-hook 'my-compile-after-hooks 'sla-etags)

  ;; sla is weird in that it must be built from the directory *above* sla
  (let ((top-dir (expand-file-name (concat dir "/.."))))

    ;; Note that compile-command will already be buffer local
    (setq compile-command (concat "make -C " top-dir " " arg))

    (set (make-local-variable 'make-clean-command)
	 (concat "make -C " top-dir " " arg "-clean " arg))))

;; *****

;; Codebase contains too much bogus whitespace
(whitespace-global-mode 0)
