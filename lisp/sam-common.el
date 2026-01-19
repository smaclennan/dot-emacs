;; -*- lexical-binding: t -*-
;;; sam-common.el --- SAM's Common macros

;; Copyright (C) 2011-2020 Sean MacLennan

(require 'cl-macs)

;;;###autoload
(defmacro my-interactive-p () `(called-interactively-p 'interactive))

;;;###autoload
(defmacro event-point (event) `(cl-cadadr ,event))

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

;;;###autoload
(defmacro strtol (str)
  "Mimic strtol(str, NULL, 0)... but not exactly."
  `(if (string-match "^\\(0[xX]\\|#x\\)" ,str)
       (string-to-number (substring ,str 2) 16)
     (string-to-number ,str)))

;;;###autoload
(defmacro find-root-dir (file-or-dir)
  "Find the directory that contains FILE-OR-DIR."
  `(let ((dir (locate-dominating-file default-directory ,file-or-dir)))
     (when dir (expand-file-name dir))))

;;;###autoload
(defmacro git-dir ()
  `(let ((dir (locate-dominating-file default-directory ".git")))
     (when dir (expand-file-name dir))))

;;;### autoload
(defmacro days-since (start &optional end)
  "How many days from START to END.
If end is nil, it is set to `current-time'.
Both START and END are in time str format."
  `(- (time-to-days (if ,end (date-to-time ,end) nil)) (time-to-days (date-to-time ,start))))

(provide 'sam-common)
