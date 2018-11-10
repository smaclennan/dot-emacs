;;; frame-utils.el --- examining XEmacs internal state

;; Copyright (C) 2006-2008 Stephen J. Turnbull

;; Author:		Stephen J. Turnbull
;; Created:		2006 August 12
;; Last-Modified:	2008 March 9
;; Keywords:		internals

;; This file is part of introspector.

;; introspector is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; introspector is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with XEmacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Synched up with: Not in GNU.

;;; Commentary:

;; TODO:
;; 1.  21.4 doesn't inhibit title bar.
;; 2.  21.4 passes focus to tip frame, so commands get eaten by it.
;;     May be due to mouse focus policy -- check it.

;;; Change Log:
;;    See accompanying ChangeLog.

;;; Code

;; frame manipulation utilities

(defun frame-remove-decoration (frame &optional inhibit-spec)
  "Return FRAME without the decorations usually added by XEmacs.

If optional argument INHIBIT-SPEC is non-nil, it must be a list of symbols
denoting types of components to remove.

Removable component types are 'modeline, 'scrollbar, 'toolbar, 'gutter,
'menubar, 'truncation-glyph, 'continuation-glyph, and 'text-cursor.

Cannot remove minibuffer/echo area; that must be done at frame creation.

Cannot inhibit window manager decorations like title and close button.
For that, either use a transient shell (set the 'popup property at frame
creation) or set the 'override-redirect property at frame creation."

  (unless inhibit-spec
    (setq inhibit-spec '(modeline scrollbar toolbar gutter truncation-glyph
			 menubar continuation-glyph text-cursor)))
  (when (and (member 'menubar inhibit-spec) (featurep 'menubar))
    ;; balloon-help.el has (set-buffer-menubar nil) but that is inappropriate
    ;; here as we haven't set a buffer yet, so some random buffer will have
    ;; its menubar nuked.
    ;; #### As of 21.5.20, there is a bug such that the frame thinks it has
    ;; a menubar, reserves space for it, and only later realizes that there
    ;; isn't one, so the first appearance of the frame will "flash" as it
    ;; resizes.  (Sorry, set-menubar-dirty doesn't help.)
    (set-specifier menubar-visible-p nil frame))
  (when (and (member 'toolbar inhibit-spec) (featurep 'toolbar))
    (set-specifier top-toolbar-height 0 frame)
    (set-specifier left-toolbar-width 0 frame)
    (set-specifier right-toolbar-width 0 frame)
    (set-specifier bottom-toolbar-height 0 frame)
    (set-specifier top-toolbar-visible-p nil frame)
    (set-specifier left-toolbar-visible-p nil frame)
    (set-specifier right-toolbar-visible-p nil frame)
    (set-specifier bottom-toolbar-visible-p nil frame)
    (set-specifier top-toolbar nil frame)
    (set-specifier left-toolbar nil frame)
    (set-specifier right-toolbar nil frame)
    (set-specifier bottom-toolbar nil frame))
  (when (and (member 'gutter inhibit-spec) (featurep 'gutter))
    (set-specifier top-gutter-height 0 frame)
    (set-specifier left-gutter-width 0 frame)
    (set-specifier right-gutter-width 0 frame)
    (set-specifier bottom-gutter-height 0 frame)
    (set-specifier top-gutter-visible-p nil frame)
    (set-specifier left-gutter-visible-p nil frame)
    (set-specifier right-gutter-visible-p nil frame)
    (set-specifier bottom-gutter-visible-p nil frame)
    (set-specifier top-gutter nil frame)
    (set-specifier left-gutter nil frame)
    (set-specifier right-gutter nil frame)
    (set-specifier bottom-gutter nil frame))
  (when (and (member 'scrollbar inhibit-spec) (featurep 'scrollbar))
    (set-specifier scrollbar-width 0 frame)
    (set-specifier scrollbar-height 0 frame))
  (when (member 'text-cursor inhibit-spec)
    (set-specifier text-cursor-visible-p nil frame))
  (when (member 'modeline inhibit-spec)
    (set-specifier has-modeline-p nil frame)
    (set-specifier modeline-shadow-thickness 0 frame))
  (when (member 'truncation-glyph inhibit-spec)
    (set-specifier (glyph-image truncation-glyph) [nothing] frame '(x)))
  (when (member 'continuation-glyph inhibit-spec)
    (set-specifier (glyph-image continuation-glyph) [nothing] frame '(x)))
  frame)

(defun make-undecorated-frame (&optional properties device)
  "Return a new unmapped frame with as few decorations as possible.
Suitable for tooltips or balloon help.  

Optional PROPERTIES is a plist of frame property-value pairs.
  Properties related to decorations will be overridden by this function.
Optional DEVICE is the device on which to make the frame, defaulting to the
  currently selected device."
  (check-valid-plist properties)
  (frame-remove-decoration
   (make-frame
    (plist-put (plist-put (plist-put (plist-put properties
						'internal-border-width 2)
				     'override-redirect t)
			  'initially-unmapped t)
	       'minibuffer 'none)
    device)))

(provide 'frame-utils)

;;; frame-utils.el ends here
