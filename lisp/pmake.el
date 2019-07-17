;;; pmake.el --- Run parallel (or not) commands.

;; Copyright (C) 2019 Sean MacLennan

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;; This is called pmake because I initially used it to run parallel
;; makes... but they can actually be any command you want. And they
;; don't need to be in parallel, it can be a bunch of sequential
;; commands. So really, pmake is a very misleading name.

;;; globals

(defvar pmake-stages nil
  "List of make commands to perform by `pmake-run'.

If a command is a simple string, run it. If the command is a
list, run all the commands in the list in parallel. The list of
commands must start with a unique string.

If a command fails, the failing command will be the car of the list.")

(defvar pmake-output-dir "/tmp/pmake/"
  "Where to put the output files.")

(defvar pmake-debug nil
  "Non-nil for debugging `pmake-run'. Will create *make dbg* buffer.")

(defvar pmake-times nil
  "Non-nil to print individual pmake times.")

;;;

(defvar pmake-start nil "Time that `pmake-run' started.")
(defvar pmake-stage-start nil "Time that this stage started.")

;;;###autoload
(defun pmake-run ()
  "Run all the commands in `pmake-stages'."
  (when pmake-debug (pmake-dump-stages))

  ;; Start by pretending to successfully finish a stage
  (setq pmake-stages (cons "ignored" pmake-stages))
  (add-hook 'compilation-finish-functions 'pmake-finish)
  (setq pmake-stage-start nil) ;; don't time first nil stage
  (pmake-finish nil "finished\n"))

(defun pmake-dump-stages ()
  (with-current-buffer (get-buffer-create "*make dbg*")
    (erase-buffer)
    (insert (format "%S\n" pmake-stages))
    (goto-char (point-min)) (forward-char)
    (while (search-forward "(" nil t) (replace-match "\n("))
    (trim-lines)))

(defun pmake-time-since (time)
  "Helper to standardize the printed time format."
  (format-time-string "%M:%S.%3N" (time-since time)))

(defun pmake-finish (buffer desc)
  (unless (equal desc "finished\n")
    (remove-hook 'compilation-finish-functions 'pmake-finish)
    (error "Stage: %s" (substring desc 0 -1)))
  (when pmake-stage-start
    (message "Stage done %s" (pmake-time-since pmake-stage-start)))
  (setq pmake-stages (cdr pmake-stages)) ;; next
  (if pmake-stages
      (let ((next (car pmake-stages)))
	(setq pmake-stage-start (current-time))
	(if (listp next)
	    (pmake-start next)
	  ;; The let binding is here to stop `compile' from setting the global var
	  (let ((compile-command next))
	    (compile compile-command)
	    (message "stage %s..." compile-command))))
    ;; Done!
    (remove-hook 'compilation-finish-functions 'pmake-finish)
    (message "Success! %s" (pmake-time-since pmake-start))
    (setq pmake-start nil)))

(defvar pmake-count 0)
(defvar pmake-rc 0)
(defvar pmake-start nil)

(defun pmake-start (list)
  (message "stage %s..." (car list))
  (setq list (cdr list))

  (setq pmake-count (length list))
  (setq pmake-rc 0)

  (setq pmake-start (current-time))
  (dolist (cmd list)
    (set-process-sentinel
     (start-process-shell-command (car cmd) nil (cadr cmd))
     'pmake-done)))

(defun pmake-done (proc desc)
  (if (equal desc "finished\n")
      (delete-file (concat pmake-output-dir (process-name proc)))
    (message "Process %S: %s" proc (substring desc 0 -1))
    (setq pmake-rc 1))
  (setq pmake-count (1- pmake-count))
  (when pmake-times
    (message "  %18s %s"
	     (process-name proc) (pmake-time-since pmake-start)))
  (when (<= pmake-count 0)
    ;; finish this stage
    (pmake-finish nil (if (eq pmake-rc 0) "finished\n" "failed\n"))))

(provide 'pmake)
