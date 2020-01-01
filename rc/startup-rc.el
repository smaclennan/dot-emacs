(setq inhibit-default-init t
      inhibit-startup-screen t
      initial-scratch-message ";; This buffer is for goofing around in.\n\n")

(require 'server)

(defun display-startup-echo-area-message ()
  "Override the Emacs default function with our friendly one."
  (interactive)
  (let ((hour (nth 2 (decode-time))))
    (message "Good %s %s"
	     (cond ((< hour 12) "morning")
		   ((< hour 18) "afternoon")
		   (t           "evening"))
	     (concat (user-full-name)
		     (if (server-running-p)
			 " with no service."
		       (server-start) nil)))))
