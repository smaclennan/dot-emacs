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

(defun region-exists-p ()
  (if mark-active
      (setq deactivate-mark t)
    nil))

(defun set-face-property (face prop arg)
  "Converts XEmacs set-face-property to `set-face-attribute'.
Not all properties are supported."
  (cond
   ((eq prop 'highlight) (setq prop :weight arg (if arg 'bold 'normal)))
   ((eq prop 'dim) (setq prop :weight arg 'light))
   ((eq prop 'underline) (setq prop :underline))
   ((eq prop 'strikethru) (setq prop :strike-through))
   ((eq prop 'reverse) (setq prop :inverse-video))
   ;; Should this be an error?
   (t (error "set-face-property prop %S not supported" prop)))
  (set-face-attribute face nil prop arg))

(global-font-lock-mode 1) ;; For 21.x

(setq inhibit-startup-echo-area-message "seanm")
(setq inhibit-startup-echo-area-message "sam")

