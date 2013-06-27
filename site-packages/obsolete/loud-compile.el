;;; -------------------------------------------------------------------------
;; Audible compilation completion
(defvar have-sound
  (and (fboundp 'device-sound-enabled-p)
       (device-sound-enabled-p))
  "* Non-nil if sound is enabled. XEmacs defaults this correctly, GNU Emacs cannot.")

(defvar loud-compile    nil   "* If t, `ding' when compile finished.")

(defun loud-finish (buff exit)
  "If `loud-compile', `ding'. Assign to `compilation-finish-function'."
  (and loud-compile
       (not (string= exit "finished\n"))
       (ding)))
(my-feature-cond
 (xemacs (setq compilation-finish-function 'loud-finish))
 (t (add-to-list 'compilation-finish-functions 'loud-finish)))

(my-feature-cond
 (xemacs
  ;; Note: XEmacs 21.5 will ding the visible bell if this funciton
  ;; returns nil, which it will on a failure.
  (defun loud-finish-fancy (buff exit)
    "If `loud-compile', `ding'. Assign to `compilation-finish-function'."
    (when loud-compile
      (let ((visible-bell nil))
	(if (string= exit "finished\n")
	    (ding nil 'compile-ok)
	  (ding nil 'compile-failed)))))

  (when have-sound
    (condition-case nil
	(progn
	  (load-sound-file "YouTheMan.au" 'compile-ok)
	  (load-sound-file "Snicker.au" 'compile-failed)
	  (setq compilation-finish-function 'loud-finish-fancy)
	  (setq loud-compile t))
      (error
       (push "Sound" would-have-liked-list))))))

