(setq igrep-verbose-prompts nil)
(put 'igrep-files-default 'c-mode (lambda () "*.[ch]"))
(put 'igrep-files-default 'emacs-lisp-mode (lambda () "*.el"))

(defun igrep-set-finish-function (func)
  (my-feature-cond
    (compilation-finish-functions
     (setq compilation-finish-functions func))
    (t
     (setq compilation-finish-function func))))

(defadvice igrep (before windowize activate)
  "This removes a final false match from `igrep' on the finished
line with `next-error'."
  (igrep-set-finish-function
   '(lambda (buf status)
      (save-excursion
	(set-buffer buf)
	(save-excursion
	  (goto-char (point-min))
	  ;; Emacs has a "started" line and will have text
	  ;; after "finished" and before "at".
	  (while (re-search-forward "^Igrep \\(started\\|finished\\) .*$" nil t)
	    (replace-match ""))))
      (igrep-set-finish-function nil))))
