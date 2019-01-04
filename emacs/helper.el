;; Check for older (21.x) GNU Emacs
(unless (featurep 'emacs) (provide 'emacs))

(defvar running-xemacs nil "Non-nil when the current emacs is XEmacs.")

;; When building outside emacs dot-dir may not be set
(defvar dot-dir (expand-file-name "~/.emacs.d/"))

;; I don't know why the hate against common-lisp
(setq byte-compile-warnings '(not cl-functions))
(require 'cl)
(require 'cl-extra)
(require 'ring)

(dolist (dir '("emacs" "lisp" "misc"))
  (add-to-list 'load-path (concat dot-dir dir))
  (load (concat dir "-loaddefs") t t))

(defmacro my-feature-cond (&rest clauses)
  "Test CLAUSES for feature, function, or variable at compile time.
Each clause is (FEATURE BODY...)."
  (dolist (x clauses)
    (let ((feature (car x))
	  (body (cdr x)))
      (when (or (eq feature t)
		(featurep feature)
		(fboundp feature)
		(boundp feature))
	(return (cons 'progn body))))))

;; I used to like when the suggestions where good, but not when they
;; are just a shortened version of the command.
(setq suggest-key-bindings nil)

(defun region-exists-p ()
  (if mark-active
      (setq deactivate-mark t)
    nil))

;; GNU Emacs really really needs a `signal-error-on-buffer-boundary'

(defadvice scroll-down (around my-scroll-down activate)
  "`scroll-down' with no signal on end-of-buffer."
  (condition-case nil
      ad-do-it
    (beginning-of-buffer)))

(defadvice scroll-up (around my-scroll-up activate)
  "`scroll-up' with no signal on end-of-buffer."
  (condition-case nil
      ad-do-it
    (end-of-buffer)))

;; Using defadvice for these functions breaks minibuffer history
(defun my-previous-line (&optional arg try-vscroll)
  "`previous-line' with no signal on end-of-buffer."
  (interactive "p")
  (condition-case nil
      (with-no-warnings ;; Yes, I want the interactive version
	(previous-line arg try-vscroll))
    (beginning-of-buffer)))

(defun my-next-line (&optional arg try-vscroll)
  "`previous-line' with no signal on end-of-buffer."
  (interactive "p")
  (condition-case nil
      (with-no-warnings ;; Yes, I want the interactive version
	(next-line arg try-vscroll))
    (end-of-buffer)))

(global-set-key (kbd "<up>") 'my-previous-line)
(global-set-key (kbd "<down>") 'my-next-line)

(defun event-point (event) (nth 1 (event-start event)))

(defun my-clipboard-copy (beg end)
  (interactive "r")
  (let ((text (buffer-substring beg end)))
    (my-feature-cond
      (gui-set-selection
       (gui-set-selection 'CLIPBOARD text) ;; for C-v
       (gui-set-selection 'PRIMARY text))  ;; for mouse paste
      (t ;; older than 25.1
       (x-set-selection 'CLIPBOARD text) ;; for C-v
       (x-set-selection 'PRIMARY text))) ;; for mouse paste
    (copy-region-as-kill beg end))) ;; and the kill buffer

(global-set-key [(shift insert)] 'x-clipboard-yank)
(global-set-key [(control insert)] 'my-clipboard-copy)

;; Hyper-apropos bindings
(define-key global-map [(control h) a] 'hyper-apropos)
(define-key global-map [(control h) c] 'hyper-describe-key-briefly)
(define-key global-map [(control h) f] 'hyper-describe-function)
(define-key global-map [(control h) k] 'hyper-describe-key)
(define-key global-map [(control h) v] 'hyper-describe-variable)
(define-key global-map [(control h) w] 'hyper-where-is)

(global-font-lock-mode 1) ;; For 21.x

(require 'etags)

(defun push-tag-mark ()
  (my-feature-cond
    (xref-push-marker-stack (xref-push-marker-stack))
    (t (ring-insert find-tag-marker-ring (point-marker)))))

;; Hacks for Emacs 23
(when (eq emacs-major-version 23)
  (mapc 'require '(git-diff my-calc my-tags smerge))
  )

(provide 'esp)
