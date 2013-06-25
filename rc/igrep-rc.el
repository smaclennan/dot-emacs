(setq igrep-verbose-prompts nil)
(put 'igrep-files-default 'c-mode (lambda () "*.[ch]"))
(put 'igrep-files-default 'emacs-lisp-mode (lambda () "*.el"))

(defadvice igrep (before windowize activate)
  "This removes a final false match from `igrep' on the finished
line with `next-error'."
  (setq compilation-finish-function
	'(lambda (buf status)
	   (save-excursion
	     (set-buffer buf)
	     (save-excursion
	       (goto-char (point-min))
	       ;; Emacs has a "started" line and will have text
	       ;; after "finished" and before "at".
	       (while (re-search-forward "^Igrep \\(started\\|finished\\) .*$" nil t)
		 (replace-match ""))))
	   (setq compilation-finish-function nil))))
