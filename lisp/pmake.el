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

;; Run all the commands in `pmake-stages'.

;;; globals

(defvar pmake-stages nil
  "List of make commands to perform by `pmake-run'.

If a command is a simple string, run it via compile with output
to the compile buffer.

If the command is a list, run all the commands in the list in
parallel. The output is thrown away unless you redirect it to a
file. The list of commands must start with a unique string.

If a command fails, the failing command will be the car of the list.

Example: Run a make clean then make subdirs lisp and misc in parallel.

(setq pmake-stages
      '(\"make clean\"
	(\"subdirs\" (\"lisp\" \"make -C lisp\") (\"misc\" \"make -C misc\"))))
(add-hook 'pmake-done-hook 'pmake-verbose-hook)
(pmake-run)")


(defvar pmake-done-hook nil
  "Hook(s) to run when pmakes done.

It will be passed three args: TYPE, DESC, PROC.
TYPE will be one of 'start, 'stage, 'pmake, 'done.
DESC will be always be set.
PROC will only be set for 'pmake.

A usable example `pmake-verbose-hook' is provided.")

(defvar pmake-debug nil
  "Non-nil for debugging `pmake-run'. Will create *pmake dbg* buffer.")

(defvar pmake-times nil
  "Non-nil to print individual pmake times.
Only used by `pmake-verbose-hook'.")

;;;

(defvar pmake-run-start nil "Time that `pmake-run' started.")
(defvar pmake-stage-start nil "Time that this stage started.")
(defvar pmake-run-rc nil "Return code for this run.")
(defvar pmake-errors-are-fatal nil)

;;;###autoload
(defun pmake-run (&optional errors-are-fatal)
  "Run all the commands in `pmake-stages'.

If ERRORS-ARE-FATAL is non-nil, then fail on the first non-zero
exit code.

At exit, `pmake-run-rc' will be t if the run was successful.

You cannot assume that `pmake-done-hook' is clean."
  (when pmake-debug
    (eval-and-compile (require 'cl-extra))
    (with-current-buffer (get-buffer-create "*pmake dbg*")
      (erase-buffer)
      (cl-prettyprint pmake-stages)))

  (setq pmake-run-start (current-time)
	pmake-run-rc t
	pmake-errors-are-fatal errors-are-fatal
	pmake-stage-start nil) ;; don't time first nil stage

  ;; Don't add the compilation hook unless we need it.
  ;; This allows us to perform compiles whilst running pmake.
  (dolist (stage pmake-stages)
    (unless (listp stage)
      (add-hook 'compilation-finish-functions 'pmake-stage-finish)))

  ;; Start by pretending to successfully finish a stage
  (setq pmake-stages (cons "ignored" pmake-stages))
  (pmake-stage-finish nil "finished\n"))

(defun pmake-stage-finish (buffer desc)
  "Stage finished sentinel function."
  (run-hook-with-args 'pmake-done-hook 'stage desc nil)
  (unless (equal desc "finished\n")
    (setq pmake-run-rc nil)
    (when pmake-errors-are-fatal
      (setq pmake-stages nil)
      (remove-hook 'compilation-finish-functions 'pmake-stage-finish)
      (run-hook-with-args 'pmake-done-hook 'done desc nil)
      (error "FAILED")))
  (setq pmake-stages (cdr pmake-stages)) ;; next
  (if pmake-stages
      (let ((next (car pmake-stages)))
	(setq pmake-stage-start (current-time))
	(if (listp next)
	    (pmake-start next)
	  ;; The let binding is here to stop `compile' from setting the global var
	  (let ((compile-command next)) (compile compile-command))
	  (run-hook-with-args 'pmake-done-hook 'start next nil)))
    ;; Done!
    (remove-hook 'compilation-finish-functions 'pmake-stage-finish)
    (unless pmake-run-rc (setq desc "failed\n"))
    (run-hook-with-args 'pmake-done-hook 'done desc nil)))

(defvar pmake-count 0)
(defvar pmake-rc nil)
(defvar pmake-pmake-start nil)

(defun pmake-start (list)
  (run-hook-with-args 'pmake-done-hook 'start (car list) nil)
  (setq list (cdr list))

  (setq pmake-count (length list))
  (setq pmake-rc t)

  (setq pmake-pmake-start (current-time))
  (dolist (cmd list)
    (set-process-sentinel
     (start-process-shell-command (car cmd) nil (cadr cmd))
     'pmake-done)))

(defun pmake-done (proc desc)
  (run-hook-with-args 'pmake-done-hook 'pmake desc proc)
  (unless (equal desc "finished\n") (setq pmake-rc nil))
  (setq pmake-count (1- pmake-count))
  (when (<= pmake-count 0)
    ;; finish this stage
    (pmake-stage-finish nil (if pmake-rc "finished\n" "failed\n"))))

(defun pmake-time-since (time)
  "Helper to standardize the printed time format."
  (format-time-string "%M:%S.%3N" (time-since time)))

;;;###autoload
(defun pmake-verbose-hook (type desc proc)
  "This is a sample `pmake-done-hook' that is very verbose."
  (cond
   ((eq type 'pmake)
    (if (equal desc "finished\n")
	(when pmake-times
	  (message "  %18s %s"
		   (process-name proc) (pmake-time-since pmake-pmake-start)))
      (message "Process %S: %s" proc (substring desc 0 -1))))
   ((eq type 'start)
    (message "stage %s..." desc))
   ((eq type 'stage)
    (if (equal desc "finished\n")
	(when pmake-stage-start
	  (message "stage done %s" (pmake-time-since pmake-stage-start)))
      (message "stage: %s" (substring desc 0 -1))))
   ((eq type 'done)
    (if pmake-run-rc
	(message "Success! %s" (pmake-time-since pmake-run-start))
      (message "FAILED.")))))

(provide 'pmake)
