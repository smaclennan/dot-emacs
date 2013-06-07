;; Windowing system only config.
;; No real library to toggle off of so it is not a -rc.el

(setq use-dialog-box nil)

(when (boundp 'xft-version)
  (set-face-font 'default "DejaVu Sans Mono-10"))

;; ---------------------------------------------
;; Colour
;;XEmacs*background: #d0ccb8
;;XEmacs*menubar.background: #607860
;;XEmacs*menubar.foreground: #d0ccb8
(when nil
  (set-face-background 'default "#d0ccb8")
  (set-face-background 'gui-button-face "#d0ccb8")
  (set-face-background 'zmacs-region "#cec180")
  ;;(set-face-background 'modeline "#ccc088")
  )
;; ---------------------------------------------

;; Performance optimizations
;; Use C-Insert and Shift-Insert for clipboard
(setq interprogram-cut-function nil
      interprogram-paste-function nil)

(my-feature-cond
  (xemacs
   (setq shifted-motion-keys-select-region t)
   (eval-when-compile (would-like 'pending-del))
   (when (would-like 'pending-del)
     (setq pending-delete-modeline-string "")
     (turn-on-pending-delete)))
  (t
   (and (< emacs-major-version 23)
	(would-like 'pc-select)
	(pc-selection-mode))))

;; Set the cursor properly for Emacs
(my-feature-cond
  (emacs
   (blink-cursor-mode 0)
   (set-cursor-color "red")))

;; -------
;; Title bar - almost every window system supports a title bar
;; The first element must be a string... sighhh.
(my-feature-cond
  (sxemacs
   (setq frame-title-format
	 '("SXEmacs " emacs-program-version "  " host-name ":"
	   (buffer-file-name "%f" "%b"))))
  (xemacs
   (setq frame-title-format
	 '("XEmacs " emacs-program-version "  " host-name ":"
	   (buffer-file-name "%f" "%b"))))
  (emacs
   (setq frame-title-format
	 '("Emacs " emacs-version "  " host-name ":"
	   (buffer-file-name "%f" "%b"))))
  (t
   (setq frame-title-format
	 '("???? " host-name ":" (buffer-file-name "%f" "%b")))))

;; -------
;; Menubar
(my-feature-cond
  (xemacs (setq menu-accelerator-enabled 'menu-fallback
		menu-accelerator-modifiers '(alt))))

;; add speedbar
(my-feature-cond
  (xemacs
   (when (packagep 'speedbar t)
     (add-menu-button '("Tools")
		      ["Speedbar" speedbar-frame-mode
		       :style toggle
		       :selected (and (boundp 'speedbar-frame)
				      (frame-live-p speedbar-frame)
				      (frame-visible-p speedbar-frame))]
		      "--"))))

;; -------
;; Toolbar
(my-feature-cond
  (xemacs (set-specifier default-toolbar-visible-p nil))
  (t (tool-bar-mode 0)))

;; -------
;; Gutter - turn it off
;; Old way
;;(when (boundp 'default-gutter-visible-p)
;;  (set-specifier default-gutter-visible-p nil))
;; New way
(when (boundp 'gutter-buffers-tab-enabled)
  (setq gutter-buffers-tab-enabled nil))

;; -------
;; Pointer used during garbage collection.
;; .xbm not supported under windoze
(my-feature-cond
  (xemacs
   (let ((img  (locate-data-file "recycle-image.xbm"))
	 (mask (locate-data-file "recycle-mask.xbm")))
     (if (and img mask (file-exists-p img) (file-exists-p mask)
	      (not running-windoze))
	 (set-glyph-image gc-pointer-glyph
			  (vector 'xbm
				  :file img
				  :mask-file mask
				  :foreground "black"
				  :background "chartreuse1"))
       (set-glyph-image gc-pointer-glyph "recycle2.xpm")))))

;; -------
;; MISC

(my-feature-cond
  (xemacs
   ;; Handy functions that where hard to work out
   (defun my-get-face-foreground (face)
     (cdr (specifier-specs (face-foreground face) 'global)))
   (defun my-get-face-background (face)
     (cdr (specifier-specs (face-background face) 'global)))
   ;; (my-get-face-background 'default)

   (defun hack-modeline-background ()
     (let ((bg (face-background-instance 'modeline)))
       (when (color-instance-p bg)
	 (set-face-background 'modeline bg))))
   (add-hook 'after-init-hook 'hack-modeline-background)))
