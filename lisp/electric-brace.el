;; electric brace pairing

(require 'cc-mode)

(defvar electric-brace-regexp "[ \t\r\n]*}[ \t\r\n]*else")

(defun insert-according-to-mode (&rest strs)
  (dolist (str strs)
    (insert str)
    (indent-according-to-mode)))

(defun electric-brace ()
  (interactive)
  (self-insert-command 1)

  (when (and (eolp)
	     ;; make sure we are not in string or comment
	     (not (nth 8 (syntax-ppss)))
	     (not (looking-at electric-brace-regexp))
	     )
    (insert-according-to-mode "" "\n" "\n}")
    (end-of-line 0)))

;;;###autoload
(defun enable-electric-brace ()
  (interactive)
  (define-key c-mode-map   "{" 'electric-brace)
  (define-key c++-mode-map "{" 'electric-brace))

;;;###autoload
(defun syntax-at-point ()
  (interactive)
  (message "%S" (syntax-ppss)))

(global-set-key "\C-cs" 'syntax-at-point)
