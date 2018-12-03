;;; motley.el --- A motley interface.
;; Copyright (C) 2018 Sean MacLennan
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

(defvar motley-prog "motley" "* The motley program.")
(defvar motley-dir nil "* Directory to start looking for motley files in.")

(require 'cl)
(require 'compile)
(require 'etags)

(defmacro motley-feature-cond (&rest clauses)
  "Test CLAUSES for feature or function at compile time.
Each clause is (FEATURE BODY...)."
  (dolist (x clauses)
    (when (or (featurep (car x)) (fboundp (car x)))
      (return (cons 'progn (cdr x))))))

(defun motley-parse-output (mode)
  "Deal with XEmacs vs GNU Emacs differences in compile"
  (compilation-mode mode)
  (motley-feature-cond
   (xemacs
    (goto-char (point-min))
    (compilation-parse-errors nil nil))
   (emacs
    ;; I tried to use compilation but it only worked 90% of the time.
    (setq buffer-read-only nil)
    (compilation--parse-region (point-min) (point-max))
    (setq buffer-read-only t)))
  (goto-char (point-min)))

(defun motley-push-tag-mark ()
  "Deal with, mainly GNU Emacs, push-tag-mark differences"
  (motley-feature-cond
   (xemacs (push-tag-mark))
   (xref-push-marker-stack (xref-push-marker-stack))
   (emacs (ring-insert find-tag-marker-ring (point-marker)))))

;;;###autoload
(defun motley-dir (&optional no-error)
  "Find the motley.out file directory. Use `motley-dir' if
set, else start looking at `default-directory'."
  (let ((dir (if motley-dir motley-dir default-directory)))
    ;; Sanitize the directory
    (setq dir (expand-file-name (file-name-as-directory dir)))
    (catch 'found
      (while (not (equal dir "/"))
	(when (file-exists-p (concat dir "motley.out"))
	  (throw 'found dir))
	;; This removes the last directory
	(setq dir (file-name-directory (directory-file-name dir))))
      (unless no-error
	(error "No motley.out file found."))
      nil)))

;;;###autoload
(defun motley (&optional sym)
  "Call motley on SYM. SYM defaults to the current word at the point.

If there is only one result, then `motley' will just goto the
symbol. If there are multiple results, you can select the result
or use `next-error' to go through the results."
  (interactive)
  (unless sym
    (let ((word (current-word)))
      (setq sym (read-string (concat "motley [" word "]: ") nil nil word))))

  (let (count (cmd (concat motley-prog " -L " sym)))
    (with-current-buffer (get-buffer-create "*motley*")
      (setq default-directory (motley-dir)) ;; must be setq
      (setq buffer-read-only nil)
      (erase-buffer)
      (insert cmd "\n\n")
      (call-process-shell-command cmd nil t)
      (motley-parse-output "motley")
      (setq count (count-lines (point-min) (point-max))))
    (motley-push-tag-mark)
    (display-buffer "*motley*" '(nil (window-height . 16)))
    (when (eq count 3) (first-error))))

;;;###autoload
(defun motley-at-point ()
  "Grab word at point and call `motley'."
  (interactive)
  (motley (current-word)))

(provide 'motley)
