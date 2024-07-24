;; -*- lexical-binding: t -*-

(require 'compile)

(defvar cov-args "--whole-program \
--include-missing-locally true \
--local-status-regex local \
--text-output-style oneline \
--print-path-events false \
--MISRA-category-regex 'Required|Mandatory'"
  "Args passed to cov-run-desktop command.")

(defun cov-run-one (what edit)
  " Helper to run one cov command.
WHAT can be:
* a file name
* --analyze-scm-modified
* --analyze-captured-source"
  (let ((curdir default-directory))
    (with-current-buffer (get-buffer-create "*cov*")
      (setq default-directory curdir)
      (erase-buffer)
      (display-buffer "*cov*")
      (let ((cmd (concat "cov-run-desktop " cov-args " " what)))
	(when edit
	  (setq cmd (read-string "Cmd: " cmd))
	  (message "%s" cmd))
	(save-excursion
	  (call-process-shell-command cmd nil t t)))

      (compilation-mode "cov")
      (setq buffer-read-only nil)
      (compilation--parse-region (point-min) (point-max))))
  (message "coverity done."))

;;## autoload
(defun cov-run (edit)
  "Run coverity on all modified files.
You must have scm and project_root set in the config file."
  (interactive "P")
  (cov-run-one "--analyze-scm-modified" edit))

;;## autoload
(defun cov-run-file (edit)
  "Run coverity on the current file only."
  (interactive "P")
  (cov-run-one buffer-file-name edit))

;;## autoload
(defun cov-run-all (edit)
  "Run coverity on all the files in this project.
All meaning the files coverity knows about."
  (interactive "P")
  (cov-run-one "--analyze-captured-source" edit))

;;## autoload
(defun cov-setup ()
  "coverity setup."
  (interactive)
  (let ((curdir default-directory))
    (with-current-buffer (get-buffer-create "*cov*")
      (setq default-directory curdir)
      (erase-buffer)
      (display-buffer "*cov*")
      (call-process "cov-run-desktop" nil t t "--setup" "--whole-program"))))
