;; Windowing system only config.
;; No real library to toggle off of so it is not a -rc.el

(setq use-dialog-box nil)

(if (boundp 'xft-version)
    (set-face-font 'default "DejaVu Sans Mono-10")
  (set-face-font 'default "7x13"))

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

;; -------
;; Title bar - almost every window system supports a title bar
;; The first element must be a string... sighhh.
(unless (boundp 'emacs-program-version) (defvar emacs-program-version emacs-version))

(defvar emacs-str (concat (if (featurep 'sxemacs) "S")
			  (if (featurep 'xemacs) "X")
			  "Emacs " emacs-program-version " " host-name ":"))

(setq frame-title-format '("" emacs-str (buffer-file-name "%f" "%b")))

;; -------
(my-feature-cond
  (xemacs
   ;; Pointer used during garbage collection.
   ;; .xbm not supported under windoze
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
       (set-glyph-image gc-pointer-glyph "recycle2.xpm")))

   ;; Menubar
   (setq menu-accelerator-enabled 'menu-fallback
	 menu-accelerator-modifiers '(alt))

   ;; Speedbar
   (when (packagep 'speedbar t)
     (add-menu-button '("Tools")
		      ["Speedbar" speedbar-frame-mode
		       :style toggle
		       :selected (and (boundp 'speedbar-frame)
				      (frame-live-p speedbar-frame)
				      (frame-visible-p speedbar-frame))]
		      "--"))

   ;; Gutter - turn it off
   (if (boundp 'gutter-buffers-tab-enabled)
       (setq gutter-buffers-tab-enabled nil)
     ;; Old way
     (if (boundp 'default-gutter-visible-p)
	 (set-specifier default-gutter-visible-p nil)))

   ;; Toolbar
   (set-specifier default-toolbar-visible-p nil))

  (t
   ;; Toolbar
   (tool-bar-mode 0)

   ;; Set the cursor properly for Emacs
   (blink-cursor-mode 0)
   (set-cursor-color "red")))

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
