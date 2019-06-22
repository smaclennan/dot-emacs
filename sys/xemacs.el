(defvar user-emacs-directory (expand-file-name "~/.xemacs/"))

(defalias 'cl-remove-if 'remove-if)

;; Needed for help
(auto-compression-mode 1)

(defmacro with-no-warnings (&rest body)
  (cons 'progn body))

(defun iswitchb-mode (arg)
  (iswitchb-default-keybindings))

(defun show-paren-mode (arg)
  (paren-set-mode 'paren t))

(defun server-start ()
  (gnuserv-start)
  (setq gnuserv-frame (selected-frame)))

;; Emacs allows a region, XEmacs does not
(defadvice count-matches (around more-args (re &optional start end) activate)
  (if start
      (save-excursion
	(goto-char start)
	(if end
	    (save-restriction
	      (narrow-to-region start end)
	      ad-do-it)
	  ad-do-it))
    ad-do-it))

;; Create the autoload files if they don't exist
(dolist (dir '("lisp" "misc"))
  (let* ((fdir (concat user-emacs-directory dir "/"))
	 (autoload-file-name (concat dir "-loaddefs.el"))
	 (generated-autoload-file (concat fdir autoload-file-name)))
    (unless (file-exists-p generated-autoload-file)
      (dolist (file (directory-files (concat user-emacs-directory dir) t ".*\\.el$"))
	(update-file-autoloads file))
      (with-current-buffer (get-buffer autoload-file-name)
	(save-buffer)))))

(global-set-key [(shift f11)] 'show-message-log)

;; Used by ws-butler
(defvar ws-butler-global-mode nil)
(defvar ws-butler-mode nil)
(defun define-globalized-minor-mode (global-mode mode turn-on &rest keys) )

;; --------------------------------------------------------------
;; Windows stuff

(require 'font-lock)

(defface font-lock-comment-warn-face
  '((((class color)) (:foreground "red" :bold t :italic t)))
  "Font Lock mode face used to highlight warning comments."
  :group 'font-lock-faces)

(setq signal-error-on-buffer-boundary nil)

(defun tool-bar-mode (arg)
  (set-specifier default-toolbar-visible-p nil))

(defun xemacs-comment-warn (keywords &optional re)
  "Helper for comment bolding. `keywords' are used by XEmacs."
  (unless re (setq re sh-comment-warn))
  (let ((keyword (list (list re 0 (quote `font-lock-comment-warn-face) t))))
    (dolist (kw keywords)
      (nconc kw keyword))))

(defun xemacs-windows-config ()
  (setq frame-title-format
	'("XEmacs " emacs-program-version " " host-name ":"
	  (buffer-file-name "%f" "%b")))


  (global-set-key (kbd "<up>") 'previous-line)
  (global-set-key (kbd "<down>") 'next-line)

  (setq shifted-motion-keys-select-region t)

  (require 'pending-del)
  (setq pending-delete-modeline-string "")
  (turn-on-pending-delete)

  ;; Gutter - turn it off
  (setq gutter-buffers-tab-enabled nil)

  ;; Toolbar
  (set-specifier default-toolbar-visible-p nil)

  (paren-activate)

  ;; Bold lisp comments
  (xemacs-comment-warn
   (list lisp-font-lock-keywords-1 lisp-font-lock-keywords-2)
   ";+ ?\\<SAM\\>.*")

  (set-face-foreground 'font-lock-comment-face "FireBrick")

  ;; Put back the original compile since Makefiles won't work
  (define-key emacs-lisp-mode-map [f7] 'emacs-lisp-byte-compile-and-load)
  )

(add-hook 'after-init-hook 'xemacs-windows-config)

(require 'mwheel)
(global-set-key [button4] 'mwheel-scroll)
(global-set-key [button5] 'mwheel-scroll)

;; SAM completely evil for laptop mode
(defvar laptop-mode-font (if (boundp 'xft-version) "Input Mono-12" "10x20")
  "*The font to use for laptop mode. (XEmacs).
When a list, it is the fonts to use for normal and laptop
mode.  Generally you don't need to setup the list, laptop-mode
will do it for you.")

(defun face-attribute (&rest args) )

(defun set-face-attribute (&rest args)
  (unless (listp laptop-mode-font)
    (setq laptop-mode-font (list (face-font-name 'default)
				 laptop-mode-font)))
  (let ((font (nth (if laptop-mode 1 0) laptop-mode-font)))
    (dolist (face (face-list))
      (set-face-font face font)))
  ;; We need a frame redraw after changing the fonts or we get
  ;; artifacts at the bottom of the display
  (redraw-frame nil t))
