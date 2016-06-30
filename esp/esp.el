;; Check for older (21.x) GNU Emacs
(unless (featurep 'emacs) (provide 'emacs))

(defvar running-xemacs nil "Non-nil when the current emacs is XEmacs.")

(require 'cl)
(add-to-list 'load-path (concat dot-dir "esp"))

;; Add the local site-packages
(let ((lisp-dir (concat dot-dir "site-packages/lisp")))
  (dolist (dir (directory-files lisp-dir t "^[^.i]"))
    (when (file-directory-p dir)
      (add-to-list 'load-path dir))))

(load "esp-loaddefs" t t)
(load "sam-loaddefs" t t)
(load "misc-loaddefs" t t)

(defun locate-data-file (name)
  ;; Try local first
  (let ((file (concat dot-dir "site-packages/etc/" name)))
    (if (file-exists-p file)
	file
      (setq file (concat data-directory name))
      (if (file-exists-p file) file nil))))

(defun emacs-version>= (major minor)
  (or (> emacs-major-version major)
      (and (eq emacs-major-version major)
	   (>= emacs-minor-version minor))))

(defun region-exists-p ()
  (if mark-active
      (setq deactivate-mark t)
    nil))

(defun set-face-property (face prop arg)
  "Converts XEmacs set-face-property to `set-face-attribute'.
Not all properties are supported."
  (cond
   ((eq prop 'highlight) (setq prop :weight arg (if arg 'bold 'normal)))
   ((eq prop 'dim) (setq prop :weight arg 'light))
   ((eq prop 'underline) (setq prop :underline))
   ((eq prop 'strikethru) (setq prop :strike-through))
   ((eq prop 'reverse) (setq prop :inverse-video))
   ;; Should this be an error?
   (t (error "set-face-property prop %S not supported" prop)))
  (set-face-attribute face nil prop arg))

;; GNU Emacs really really needs a `signal-error-on-buffer-boundary'
(defun my-scroll-down (&optional arg)
  "`scroll-down-command' with no signal on beginning-of-buffer."
  (interactive "P")
  (condition-case nil
      (scroll-down arg)
    (beginning-of-buffer)))

(defun my-scroll-up (&optional arg)
  "`scroll-up-command' with no signal on end-of-buffer."
  (interactive "P")
  (condition-case nil
      (scroll-up arg)
    (end-of-buffer)))

(defun my-previous-line (&optional arg)
  "`previous-line' with no signal on beginning-of-buffer."
  (interactive "p")
  (condition-case nil
      (previous-line arg)
    (beginning-of-buffer)))

(defun my-next-line (&optional arg)
  "`next-line' with no signal on end-of-buffer."
  (interactive "p")
  (condition-case nil
      (next-line arg)
    (end-of-buffer)))

(defun my-shift-select-down ()
  (interactive)
  (unless (region-active-p) (set-mark-command nil))
  (my-next-line))

(defun my-shift-select-up ()
  (interactive)
  (unless (region-active-p) (set-mark-command nil))
  (my-previous-line))

(global-set-key (kbd "<prior>") 'my-scroll-down)
(global-set-key "\M-v" 'my-scroll-down)
(global-set-key (kbd "<next>") 'my-scroll-up)
(global-set-key "\C-v" 'my-scroll-up)
(global-set-key (kbd "<up>") 'my-previous-line)
(global-set-key "\C-p" 'my-previous-line)
(global-set-key (kbd "<down>") 'my-next-line)
(global-set-key "\C-n" 'my-next-line)
(global-set-key [S-down] 'my-shift-select-down)
(global-set-key [S-up] 'my-shift-select-up)

(defun my-clipboard-copy (beg end)
  (interactive "r")
  (let ((text (buffer-substring beg end)))
    (x-set-selection 'CLIPBOARD text) ;; for C-v
    (x-set-selection 'PRIMARY text) ;; for mouse paste
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

(setq inhibit-startup-echo-area-message "seanm")
(setq inhibit-startup-echo-area-message "sam")

;; Let's see how we like this. Unfortunately it also stops at the
;; first warning. Which may be irritating.
(setq compilation-scroll-output 'first-error)

(eval-when-compile (require 'etags))

;; Mimics version from XEmacs 21.2
(defun find-tag-at-point ()
  "*Find tag whose name contains TAGNAME.
Identical to `find-tag' but does not prompt for tag when called interactively;
instead, uses tag around or before point."
  (interactive)
  (find-tag (if current-prefix-arg
		(find-tag-tag "Find tag: "))
	    (find-tag (find-tag-default))))

;; Hacks for Emacs 23
(when (eq emacs-major-version 23)
  (mapcar 'require '(git-diff my-calc my-tags smerge))
  )
