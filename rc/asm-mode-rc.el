(add-hook 'asm-mode-hook 'my-compile-command)

;; We only want _ as a symbol constituent
(with-syntax-table asm-mode-syntax-table
  (cl-loop for char from ?! to ?~ unless (eq char ?_) do
	   (when (eq ?_ (char-syntax char))
	     (modify-syntax-entry char "." ))))

;; This should be the `asm-calculate-indention' function with the
;; addition of dot commands
(defun asm-calculate-indentation ()
  (or
   ;; Flush labels to the left margin.
   (and (looking-at "\\(\\sw\\|\\s_\\)+:") 0)
   ;; Same thing for `;;;' comments.
   (and (looking-at "\\s<\\s<\\s<") 0)
   ;; SAM Same thing for dot commands
   (and (looking-at "\\(\\s_\\)*\\.") 0)
   ;; Simple `;' comments go to the comment-column.
   (and (looking-at "\\s<\\(\\S<\\|\\'\\)") comment-column)
   ;; The rest goes at the first tab stop.
   (indent-next-tab-stop 0)))
