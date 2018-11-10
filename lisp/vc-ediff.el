;;; vc-ediff.el --- Replacement for vc-diff

;; Copyright (C) 2004-2010 Sean MacLennan
;; XEmacs

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


(defvar vc-ediff-buffer-list nil "Local variable.")
(defvar vc-ediff-delete nil "Local variable.")

(eval-when-compile (require 'ediff))

;; vc-version-other-window does the heavy lifting.
;; Emacs 23 uses vc-revision rather than vc-version.
;; Thanks to Andrew Stein for catching this.
(defmacro vc-ediff-version-other-window (rev)
  (if (and (featurep 'emacs) (>= emacs-major-version 23))
      '(vc-revision-other-window rev)
    '(vc-version-other-window rev)))

;;;###autoload
(defun vc-ediff (arg)
  "Like `vc-diff' but performs an `ediff' rather than a `diff'."
  (interactive "P")
  (let ((work (current-buffer))
	(mode major-mode)
	(rev "")
	cvs)
    (when arg (setq rev (read-string "Revision: ")))
    (setq vc-ediff-delete nil)
    (vc-ediff-version-other-window rev)
    (setq major-mode mode)
    (setq cvs (current-buffer))
    (add-to-list 'vc-ediff-buffer-list cvs)
    (add-hook 'ediff-quit-hook 'vc-ediff-quit-hook)
    (add-hook 'ediff-quit-hook 'vc-ediff-cleanup t)
    (ediff-buffers (buffer-name work) (buffer-name cvs))))

;; Cleanup the file?
(defun vc-ediff-quit-hook ()
  (when (memq ediff-buffer-B vc-ediff-buffer-list)
    (setq vc-ediff-delete (list ediff-buffer-A ediff-buffer-B))))

(defun vc-ediff-cleanup ()
  (when vc-ediff-delete
    (switch-to-buffer (car vc-ediff-delete))
    (kill-buffer (cadr vc-ediff-delete))
    (delete-other-windows)
    (setq vc-ediff-buffer-list (delq vc-ediff-delete vc-ediff-buffer-list))
    (unless vc-ediff-buffer-list
      (remove-hook 'ediff-quit-hook 'vc-ediff-quit-hook)
      (remove-hook 'ediff-quit-hook 'vc-ediff-cleanup))
    (setq vc-ediff-delete nil)
    ))

(provide 'vc-ediff)
