(defun locate-data-file (name)
  ;; Try local first
  (let ((file (concat dot-dir "site-packages/etc/" name)))
    (if (file-exists-p file)
	file
      (setq file (concat data-directory name))
      (if (file-exists-p file) file nil))))

(defun emacs-version>= (major minor)
  (or (> major emacs-major-version)
      (and (= major emacs-major-version)
	   (>= minor emacs-minor-version))))

(setq inhibit-startup-echo-area-message "seanm")

