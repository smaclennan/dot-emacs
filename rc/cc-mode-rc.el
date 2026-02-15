;; -*- lexical-binding: t -*-
;;; -------------------------------------------------------------------------
;; CC-MODE
;; Customizations for c-mode, c++-mode, java-mode, etc.

;; Same as Linux except 4 char tabs
(c-add-style "sam" '("linux" (c-basic-offset . 4) (tab-width . 4)))

;; This hook is run for all the modes handled by cc-mode
(defun my-c-mode-common-hook ()
  (c-set-style "sam")
  (c-toggle-hungry-state 1)  ;; hungry delete
  (c-toggle-electric-state 1) ;; auto-indent
  (setq c-tab-always-indent 'other) ;; real tabs in strings and comments
  (setq case-fold-search nil) ;; C is case sensitive

  ;; _ is part of a word
  (modify-syntax-entry ?_ "w" c-mode-syntax-table)
  ;;(superword-mode)
  
  (let ((tags (expand-file-name "TAGS")))
    (if (file-exists-p tags) (visit-tags-table tags t)))

  (my-compile-command))
(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

(require 'my-compile)

;; Turn off gcc colours
(setenv "GCC_COLORS" "")

;; Some safe local variables
(put 'compile-command 'safe-local-variable #'stringp)
(put 'my-checkpatch-ignores 'safe-local-variable #'stringp)

;; Bold SAM comments
(dolist (mode '(c-mode c++-mode))
  (comment-warn mode "\\(/\\*\\|//\\) ?\\<SAM\\>.*"))
