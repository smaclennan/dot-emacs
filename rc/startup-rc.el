(setq inhibit-default-init t
      inhibit-startup-screen t
      initial-scratch-message ";; This buffer is for goofing around in.\n\n"
      ;; These are actually from simple.el...
      track-eol t
      kill-whole-line t
      extended-command-suggest-shorter nil)

;; These just seem to belong here.
(put 'narrow-to-region 'disabled nil) ;; Why? Just why?
(fset 'yes-or-no-p 'y-or-n-p)

;; Collect the auto-save and backup files in one place.
;; I added the following to my crontab:
;; 17 5 * * * find $HOME/.autosave -mtime +7 -delete
(make-directory "~/.autosave" t)
(setq auto-save-list-file-prefix nil) ;; don't create auto-save-list directory
(setq auto-save-file-name-transforms `((".*" "~/.autosave/" t)))
;; Not really part of startup... but belongs with auto-save
(setq backup-directory-alist '((".*" . "~/.autosave")))

(require 'server)

(defun display-startup-echo-area-message ()
  "Override the Emacs default function with our friendly one."
  (interactive)
  (run-at-time 0.250 nil
	       (lambda ()
		 (let ((hour (nth 2 (decode-time))))
		   (message "Good %s %s"
			    (cond ((< hour 12) "morning")
				  ((< hour 18) "afternoon")
				  (t           "evening"))
			    (concat (user-full-name)
				    (if (server-running-p)
					" with no service."
				      (server-start) nil)))))))
