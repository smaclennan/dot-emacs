;; -*- lexical-binding: t -*-
(add-hook 'asm-mode-hook 'my-compile-command)

;; We only want _ as a symbol constituent
(with-syntax-table asm-mode-syntax-table
  (cl-loop for char from ?! to ?~ unless (eq char ?_) do
	   (when (eq ?_ (char-syntax char))
	     (modify-syntax-entry char "." ))))

;; .if/.else/.endif left justified
(advice-add 'asm-calculate-indentation :filter-return
	    (lambda(x) (if (looking-at "\\(\\s_\\)*\\.\\(if\\|else\\|endif\\)") 0 x)))

;; Bold SAM C style comments
(comment-warn 'asm-mode "/\\* ?\\<SAM\\>.*")
