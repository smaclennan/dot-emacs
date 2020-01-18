;;; sam-common.el --- SAM's Common macros

;; Copyright (C) 2011-2020 Sean MacLennan

;;;###autoload
(defmacro my-interactive-p () `(called-interactively-p 'interactive))

;;;###autoload
(defmacro event-point (event) `(cl-cadadr event))

;;;###autoload
(defmacro basename (name)
  `(if (string-match "/\\([^/]+\\)/?$" ,name)
       (match-string 1 ,name)
     ,name))

;;;###autoload
(defmacro measure-time (loops &rest body)
  "Measure the time it takes to evaluate BODY LOOPS times."
  `(let ((time (current-time)))
     (cl-loop repeat ,loops do ,@body)
     (message "%.06f" (float-time (time-since time)))))

;; This is almost 4x faster than shell-command-to-string
;;;###autoload
(defmacro uname (arg)
  `(with-temp-buffer
    (call-process "uname" nil t nil ,arg)
    (buffer-substring (point-min) (1- (point-max)))))

;; For some reason this needs to be a function for arm Linux
;;;###autoload
(defmacro strtol (str)
  "Mimic strtol(str, NULL, 0)... but not exactly"
  `(if (string-match "^0[xX]\\(.*\\)" ,str)
       ;; match-string does not work on arm Linux
       (string-to-number (substring ,str 2) 16)
     (string-to-number ,str)))

;;;###autoload
(defun find-root-dir (file-or-dir &optional dir)
  "Find the directory that contains FILE-OR-DIR.
Starts at DIR (defaults to `default-directory') and goes up the path."
  (unless dir (setq dir default-directory))
  ;; Sanitize the directory
  (setq dir (expand-file-name (file-name-as-directory dir)))
  (catch 'found
    (while (not (equal dir "/"))
      (when (file-exists-p (concat dir file-or-dir))
	(throw 'found dir))
      ;; This removes the last directory
      (setq dir (file-name-directory (directory-file-name dir))))))

(provide 'sam-common)
