(defvar my-grep-prog "grep -nH"
  "*The grep program")

(defvar my-grep-regex-history nil
  "The minibuffer history list for `my-grep' and `my-grep-find's REGEX argument.")

(defvar my-grep-files-history nil
  "The minibuffer history list for `my-grep' and `my-grep-find's FILES argument.")

(defun my-grep-exts ()
  (let ((ext (if buffer-file-name (file-name-extension buffer-file-name))))
    (if ext
	;; This is my preference... and it is my code.
	(if (or (string= ext "c") (string= ext "h"))
	    "*.[ch]"
	  (concat "*." ext))
      "*")))

;;;###autoload
(defun my-grep (regex files)
  "Run the Emacs `grep' command with some nicer defaults. The
REGEX defaults to the word under the point. FILES defaults to the
current extension.

With a prefix arg you can edit the grep command before the grep
is run. This allows extra flags like -i etc. Don't remove the -nH or
`next-error' will not work.

Non-interactively this is just:
(grep (concat my-grep-prog \" \\\"\" regex \"\\\" \" files))"
  (interactive
   (let* ((word (current-word))
	  (exts (my-grep-exts))
	  (rprompt (concat "Regex [" word "]: "))
	  (fprompt (concat "Files [" exts "]: ")))
     (list (read-string rprompt nil 'my-grep-regex-history word)
	   (read-string fprompt nil 'my-grep-files-history exts))))
  (let ((cmd (concat my-grep-prog " \"" regex "\" " files)))
    (when current-prefix-arg
      (setq cmd (read-string "Cmd: " cmd 'grep-history)))
    (grep cmd)))

;;;###autoload
(defun my-grep-find (regex files)
  "Run the Emacs `grep' command with some nicer defaults. The
REGEX defaults to the word under the point. FILES defaults to the
current extension. The unix find command is used to recursively
find all files.

With a prefix arg you can edit the grep command before the grep
is run. This allows extra flags like -i etc. It is generally a
VBI (Very Bad Idea) to remove any of the existing flags.

Non-interactively this is just:
(grep (concat \"find -name '\" files \"' -print0 |
              xargs -0 \" my-grep-prog \" '\" regex \"'\"))"
  (interactive
   (let* ((word (current-word))
	  (exts (my-grep-exts))
	  (rprompt (concat "Regex [" word "]: "))
	  (fprompt (concat "Files [" exts "]: ")))
     (list (read-string rprompt nil 'my-grep-regex-history word)
	   (read-string fprompt nil 'my-grep-files-history exts))))
  (let ((cmd (concat "find -name '" files "' -print0 | xargs -0 " my-grep-prog " '" regex "'")))
    (when current-prefix-arg
      (setq cmd (read-string "Cmd: " cmd 'grep-history)))
    (grep cmd)))
