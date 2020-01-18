;;;; Function keys.
;; Shifted function keys: [(shift f1)] == [XF86_Switch_VT_1]
(global-set-key [f1]            'find-file)
(global-set-key [(shift f1)]    'update-local-compile-command)
(global-set-key [(control f1)]  'add-local-compile-command)
(global-set-key [f2]		'undo)
(global-set-key [f3]		'isearch-repeat-forward)
(global-set-key [(shift f3)]    'isearch-repeat-backward)
(global-set-key [f4]		'next-error)
(global-set-key [f5]		'query-replace)
(global-set-key [(shift f5)]    'query-replace-regexp)
(global-set-key [f6]		'git-grep-at-point)
(global-set-key [(shift f6)]	'git-grep)
(global-set-key [(control f6)]	'git-grep-toggle-top-of-tree)
(global-set-key [f7]		'compile)
(global-set-key [(shift f7)]    'my-make-clean)
(global-set-key [(control f7)]	'my-set-compile)
(global-set-key [f8]		'my-grep-i-feel-lucky)
(global-set-key [(shift f8)]	'my-grep)
(global-set-key [(control f8)]	'my-grep-find)
(global-set-key [f9]		'my-isearch-word-forward)
(global-set-key [(shift f9)]    'my-toggle-case-search)
(global-set-key [(control f9)]	'my-checkpatch)
(global-set-key [f10]		'xref-find-definitions)
(global-set-key [(shift f10)]   'pop-tag-mark)
(global-set-key [(control f10)] 'xref-find-references)
(global-set-key [f11] nil) ;; I keep f11 free for temporary bindings
(global-set-key [(shift f11)]	(lambda () (interactive)
				  (switch-to-buffer-other-window "*Messages*")))
(global-set-key [f12]		'revert-buffer)
(global-set-key [(shift f12)]	'lxr-next-defined)
(global-set-key [(control f12)] 'lxr-defined-at-point)

(global-set-key "\M-."		'xref-find-definitions-prompt)
(global-set-key [(meta right)]	'forward-sexp)
(global-set-key [(meta left)]	'backward-sexp)


(global-set-key "\C-x\C-l"	'list-buffers)
(global-set-key "\C-x\C-k"	'kill-buffer)
(global-set-key "\C-xw"		'what-line)
(global-set-key "\M-#"		'my-calc)
(global-set-key [(iso-left-tab)] 'tab-to-tab-stop)

(global-set-key "\C-cd" 'dup-line)
(global-set-key "\C-ce" 'errno-string)
(global-set-key "\C-cg" 'git-diff)
(global-set-key "\C-ci" 'tag-includes)
(global-set-key "\C-co" 'ogrok)

(global-set-key "\C-c\C-t" 'swap-in-word)

;; For some reason this doesn't have a key binding
(global-set-key "\C-hz" 'apropos-variable)

;; Using defadvice for these functions breaks minibuffer history
(defun my-previous-line (arg)
  "`previous-line' with no signal on beginning-of-buffer."
  (interactive "p")
  (line-move (- arg) t))

(defun my-next-line (arg)
  "`next-line' with no signal on end-of-buffer."
  (interactive "p")
  (line-move arg t))

(global-set-key [up]   'my-previous-line)
(global-set-key [down] 'my-next-line)
(global-set-key "\C-p" 'my-previous-line)
(global-set-key "\C-n" 'my-next-line)

(defadvice scroll-down (around my-scroll-down activate)
  "`scroll-down' with no signal on end-of-buffer."
  (condition-case nil
      ad-do-it
    (error (goto-char (point-min)))))

(defadvice scroll-up (around my-scroll-up activate)
  "`scroll-up' with no signal on end-of-buffer."
  (condition-case nil
      ad-do-it
    (error (goto-char (point-max)))))
