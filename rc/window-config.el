;; Windowing system only config.
;; No real library to toggle off of so it is not a -rc.el

(defvar bells-and-whistles nil "*If non-nil, enable all graphical widgets.")

(when nil ;; Disable for now
  (defvar x-root-size nil "X root window width and height")

  (when (eq window-system 'x)
    (when (eq x-root-size nil)
      (let ((wininfo (shell-command-to-string "xwininfo -root"))
	    width height)
	(when (string-match "Width: \\([0-9]+\\)" wininfo)
	  (setq width (string-to-number (match-string 1 wininfo))))
	(when (string-match "Height: \\([0-9]+\\)" wininfo)
	  (setq height (string-to-number (match-string 1 wininfo))))
	(and width height (setq x-root-size (list width height)))))))

;; GNU Emacs
;;(set-face-attribute 'default nil :family "Input Mono" :foundry "unknown"
;;		    :slant 'normal :weight 'extra-light :height 98 :width 'normal)

(my-feature-cond
  (xemacs
   ;; Performance optimizations
   ;; Use C-Insert and Shift-Insert for clipboard
   (setq interprogram-cut-function nil
	 interprogram-paste-function nil)

   (setq shifted-motion-keys-select-region t)
   (eval-when-compile (would-like 'pending-del))
   (when (would-like 'pending-del)
     (setq pending-delete-modeline-string "")
     (turn-on-pending-delete)))
  (delete-selection-mode
   (delete-selection-mode))
  (emacs
   (and (< emacs-major-version 23)
	(would-like 'pc-select)
	(pc-selection-mode))))

;; -------
;; Title bar - almost every window system supports a title bar
;; The first element must be a string... sighhh.
(defvar emacs-str (concat
		   (if (featurep 'sxemacs) "S")
		   (if (featurep 'xemacs)  "X")
		   "Emacs "
		   (if running-xemacs emacs-program-version emacs-version)
		   " " host-name ":"))

(if running-xemacs
    (setq frame-title-format '("" emacs-str (buffer-file-name "%f" "%b")))
  (setq frame-title-format '("" emacs-str
			     (buffer-file-name
			      (:eval (abbreviate-file-name buffer-file-name))
			      "%b"))))

;; -------
(my-feature-cond
  (xemacs
   ;; Pointer used during garbage collection.
   ;; .xbm not supported under windoze
   (let (img mask)
     (if (string= (x-server-vendor) "Colin Harrison")
	 ;; xming only supports 32x32
	 (setq img  (locate-data-file "recycle-image-32.xbm")
	       mask (locate-data-file "recycle-mask-32.xbm"))
       (setq img  (locate-data-file "recycle-image.xbm")
	     mask (locate-data-file "recycle-mask.xbm")))
     (if (and img mask (not running-windoze))
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
   (when (and bells-and-whistles (would-like 'speedbar t))
     (add-menu-button '("Tools")
		      ["Speedbar" speedbar-frame-mode
		       :style toggle
		       :selected (and (boundp 'speedbar-frame)
				      (frame-live-p speedbar-frame)
				      (frame-visible-p speedbar-frame))]
		      "--"))

   (unless bells-and-whistles
     (setq use-dialog-box nil)

     ;; Gutter - turn it off
     (if (boundp 'gutter-buffers-tab-enabled)
	 (setq gutter-buffers-tab-enabled nil)
       ;; Old way
       (if (boundp 'default-gutter-visible-p)
	   (set-specifier default-gutter-visible-p nil)))

     ;; Toolbar
     (set-specifier default-toolbar-visible-p nil))
   ) ;; xemacs

  (emacs
   (unless bells-and-whistles
     (setq use-dialog-box nil)
     ;; Toolbar
     (tool-bar-mode 0)
     ;; Tooltips hang emacs over VPN
     (tooltip-mode 0))

   ;; Set the cursor properly for Emacs
   (blink-cursor-mode 0)
   (set-cursor-color "red")))

;; Do this *after* setting the modeline colours
(and nil (fboundp 'display-time) (display-time))

;; Deal with laptop-mode after all fonts setup
(load (concat rcfiles-directory "laptop-mode"))
