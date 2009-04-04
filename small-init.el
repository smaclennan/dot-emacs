;; X?Emacs setup -*- Mode:emacs-lisp -*-
;; This file should work with Emacs 21.2.x/XEmacs 21.4.x

(unless (boundp 'running-xemacs)
  (defvar running-xemacs nil))

(setq track-eol t)
(setq kill-whole-line t)
(setq next-line-add-newlines nil)

(put 'narrow-to-region 'disabled nil)

;; Turn on parenthesis matching mode
(if running-xemacs
    (paren-set-mode 'paren t)
  (show-paren-mode))

;; This gives a PC like shift to select. If you select text and then
;; insert, the selected text is killed.
(if running-xemacs
    (progn
      (setq shifted-motion-keys-select-region t)
      (require 'pending-del)
      (setq pending-delete-modeline-string "")
      (turn-on-pending-delete))
  (pc-selection-mode))

;;; ------------------------------------------------------------------
;; Key Bindings

;; These map to Visual Studio.
(global-set-key [f3]		'isearch-repeat-forward)
(global-set-key [(shift f3)]	'isearch-repeat-backward)
(global-set-key [f4]		'next-error)
(global-set-key [f7]		'compile)

;; The following compilation variables make the compile command less
;; verbose. This means that the f7 key works just like in VS with no
;; questions asked.

;; Do not ask for compile command unless a universal arg (C-u) is
;; specified
(setq compilation-read-command   nil)

;; Save *all* files without asking. I love this feature but some
;; people hate it. Caveat Emptor.
(setq compilation-ask-about-save nil)

;; I prefer iswitchb over switch-to. You can compare it to incremental
;; search versus plain search.
(iswitchb-default-keybindings)

; I also tend to keep the Ctrl key down, so map this to that either
; C-x b or C-x C-b works. Since C-x C-b is usually list buffers, I
; bind that to the more mnemonic Cx C-l.
(global-set-key "\C-x\C-b"	'iswitchb-buffer)
(global-set-key "\C-x\C-l"	'list-buffers)

;;; ------------------------------------------------------------------
;; Font Lock - syntax highlighting

(require 'font-lock)

(unless running-xemacs
  (global-font-lock-mode))

;; Maximum colour but minimum chatter
(setq-default font-lock-maximum-decoration t
	      font-lock-verbose nil)

;;; ------------------------------------------------------------------
;; CC-MODE
;; Customizations for c-mode, c++-mode, java-mode, etc.

;; This hook is run once when cc-mode initializes
(defun my-c-initialization-hook ()
  (define-key c-mode-base-map [(return)] 'newline-and-indent)
  )
(add-hook 'c-initialization-hook 'my-c-initialization-hook)

;; This hook is run for all the modes handled by cc-mode
;; This is a style for at work. I do not recommend it.
(defun my-c-mode-common-hook ()
  (unless (assoc "pika" c-style-alist)
    (c-add-style "pika"
		 '("gnu"
		   (c-offsets-alist . ((substatement-open   . 0)
				       (statement-case-open . 0)
				       (case-label . +))))))
  (c-set-style "pika")
  (setq tab-width 2))
(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

;;; ------------------------------------------------------------------
;; igrep mode - Use f4 to walk through the results.

(when running-xemacs
  (require 'igrep)
  ;;(igrep-insinuate)
  (global-set-key [f8] 'igrep)
  (global-set-key [(shift f8)] 'igrep-find)
  (setq igrep-verbose-prompts nil)
  (put 'igrep-files-default 'c-mode (lambda () "*.[ch]"))
  (put 'igrep-files-default 'emacs-lisp-mode (lambda () "*.el")))

