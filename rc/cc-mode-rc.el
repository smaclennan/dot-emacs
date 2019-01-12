;;; -------------------------------------------------------------------------
;; CC-MODE
;; Customizations for c-mode, c++-mode, java-mode, etc.

;; Let's try making _ part of a "word"
(modify-syntax-entry ?_ "w" c-mode-syntax-table)
(modify-syntax-entry ?_ "w" c++-mode-syntax-table)

;; Same as Linux except 4 char tabs
(c-add-style "sam" '("linux" (c-basic-offset . 4) (tab-width . 4)))

;; This hook is run for all the modes handled by cc-mode
(defun my-c-mode-common-hook ()
  (c-set-style "sam")
  (c-toggle-hungry-state 1)  ;; hungry delete
  (c-toggle-electric-state 1) ;; auto-indent
  (setq c-tab-always-indent 'other) ;; real tabs in strings and comments
  (setq case-fold-search nil) ;; C is case sensitive

  (let ((tags (concat default-directory "TAGS")))
    (when (file-exists-p tags)
      (set (make-local-variable 'tags-file-name) tags)))
  )
(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

(defun set-c-vars (tabs width)
  "Set C tab mode and tab width"
  (interactive (list
		(yes-or-no-p "Tabs? ")
		(read-number "Width: " c-basic-offset)))
  (setq indent-tabs-mode tabs
	c-basic-offset width
	tab-width width))

(would-like 'my-compile)

;; Turn off gcc colours
(setenv "GCC_COLORS" "")

;; Just allow compile commands
(defun cc-string (obj) (stringp obj))
(put 'compile-command 'safe-local-variable #'cc-string)

;;; -------------------------------------------------------------------------
;; Bold SAM comments

(comment-warn
 'c-mode
 "\\(/\\*\\|//\\) ?\\<SAM\\>.*")
(comment-warn
 'c++-mode
 "\\(/\\*\\|//\\) ?\\<SAM\\>.*")
