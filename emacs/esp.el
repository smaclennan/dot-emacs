;; Check for older (21.x) GNU Emacs
(unless (featurep 'emacs) (provide 'emacs))

(defvar running-xemacs nil "Non-nil when the current emacs is XEmacs.")

;; When building outside emacs dot-dir may not be set
(if (not (boundp 'dot-dir))
    (setq dot-dir (expand-file-name "~/.emacs.d/")))

;; I don't know why the hate against common-lisp
(setq byte-compile-warnings '(not cl-functions))
(require 'cl)
(require 'cl-extra)

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file t)

(dolist (dir '("emacs" "lisp" "misc"))
  (add-to-list 'load-path (concat dot-dir dir)))

(dolist (file '("emacs-loaddefs" "lisp-loaddefs" "misc-loaddefs"))
  (load file t t))

;; I used to like when the suggestions where good, but not when they
;; are just a shortened version of the command.
(setq suggest-key-bindings nil)

(defun emacs-version>= (major minor)
  (or (> emacs-major-version major)
      (and (eq emacs-major-version major)
	   (>= emacs-minor-version minor))))

(defmacro emacs-version-cond (&rest clauses)
  "Test CLAUSES for version >= at compile time.
Each clause is (VERSION BODY...).
Where VERSION is a list of major minor (e.g. (25 1)) or t."
  (dolist (x clauses)
    (let ((v (car x))
	  (body (cdr x)))
      (when (or (eq v t)
		;; Unroll emacs-verion>= or we get void function definiton
		(> emacs-major-version (car v))
		(and (eq emacs-major-version (car v))
		     (>= emacs-minor-version (cadr v))))
	(return (cons 'progn body))))))
(put 'emacs-version-cond 'lisp-indent-hook 'defun)

(defun locate-data-file (name)
  ;; Try local first
  (let ((file (concat dot-dir "etc/" name)))
    (if (file-exists-p file)
	file
      (setq file (concat data-directory name))
      (if (file-exists-p file) file nil))))

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


(defun my-clipboard-copy (beg end)
  (interactive "r")
  (let ((text (buffer-substring beg end)))
    (emacs-version-cond
      ((25 1)
       (gui-set-selection 'CLIPBOARD text) ;; for C-v
       (gui-set-selection 'PRIMARY text))  ;; for mouse paste
      (t
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

(eval-when-compile (require 'etags))

;; Mimics version from XEmacs 21.2
(defun find-tag-at-point ()
  "*Find tag whose name contains TAGNAME.
Identical to `find-tag' but does not prompt for tag when called interactively;
instead, uses tag around or before point."
  (interactive)
  (find-tag (if current-prefix-arg
		(find-tag-tag "Find tag: ")
	      (find-tag-default))))

(defun push-tag-mark () (ring-insert find-tag-marker-ring (point-marker)))

;; Hacks for Emacs 23
(when (eq emacs-major-version 23)
  (mapc 'require '(git-diff my-calc my-tags smerge))
  )

(provide 'esp)