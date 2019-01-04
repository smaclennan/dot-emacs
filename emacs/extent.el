;;; extent.el - extent compatibility for Emacs

;; Copyright (C) 2002-2010 Sean MacLennan
;; Revision:   1.2
;; Emacs

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABIL`ITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.


;; These extents *only* work on buffers, not strings

(require 'overlay)

;; Currently only returns first overlay in list...
;; Not correct but works for slashdot.el and lxr.el
;; incomplete (very)
(defun extent-at (pos &optional object)
  (save-excursion
    (and object
	 (bufferp object)
	 (set-buffer object))
    (let ((overlays (overlays-at pos)))
      (car overlays))))

;; SAM ???
(defun next-extent (extent)
  (let* ((pos (1- (overlay-end extent)))
	 (next (next-overlay-change pos))
	 (overlay (overlays-at next)))
    (when overlay
      (setq overlay (car overlay))
      (when (eq overlay extent) (error "PROBLEMS"))
      )
    overlay))

;; These should really not be here. But I use it with extents.
;; complete
(defun event-point (event) (nth 1 (event-start event)))

(provide 'extent)
