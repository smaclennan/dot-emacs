;; Windowing system only config.
;; No real library to toggle off of so it is not a -rc.el

(defvar bells-and-whistles nil "*If non-nil, enable all graphical widgets.")

(my-feature-cond
  (xemacs
   (setq frame-title-format
	 '("XEmacs " emacs-program-version " " host-name ":"
	   (buffer-file-name "%f" "%b")))

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

   ;; Performance optimizations
   ;; Use C-Insert and Shift-Insert for clipboard
   (setq interprogram-cut-function nil
	 interprogram-paste-function nil)

   (setq shifted-motion-keys-select-region t)
   (eval-when-compile (would-like 'pending-del))
   (when (would-like 'pending-del)
     (setq pending-delete-modeline-string "")
     (turn-on-pending-delete))

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
   (setq frame-title-format '("Emacs " emacs-version " " host-name ":"
			      (buffer-file-name
			       (:eval (abbreviate-file-name buffer-file-name))
			       "%b")))

   (if (fboundp 'delete-selection-mode)
       (delete-selection-mode)
     (and (< emacs-major-version 23)
	  (would-like 'pc-select)
	  (pc-selection-mode)))

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
