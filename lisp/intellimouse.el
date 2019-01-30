;;; intellimouse.el - do something with the scroll wheel

;; Copyright (C) 2000-2017 Sean MacLennan

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

(defvar fine-scroll-lines 4 "*Number of lines to fine scroll")

;; GNU Emacs really really needs a `signal-error-on-buffer-boundary'

;; Using defadvice for these functions breaks minibuffer history
(defun my-previous-line (&optional arg try-vscroll)
  "`previous-line' with no signal on end-of-buffer."
  (interactive "p")
  (condition-case nil
      (with-no-warnings ;; Yes, I want the interactive version
	(previous-line arg try-vscroll))
    (beginning-of-buffer)))

(defun my-next-line (&optional arg try-vscroll)
  "`previous-line' with no signal on end-of-buffer."
  (interactive "p")
  (condition-case nil
      (with-no-warnings ;; Yes, I want the interactive version
	(next-line arg try-vscroll))
    (end-of-buffer)))

(global-set-key (kbd "<up>") 'my-previous-line)
(global-set-key (kbd "<down>") 'my-next-line)

(defadvice scroll-down (around my-scroll-down activate)
  "`scroll-down' with no signal on end-of-buffer."
  (condition-case nil
      ad-do-it
    (beginning-of-buffer)))

(defadvice scroll-up (around my-scroll-up activate)
  "`scroll-up' with no signal on end-of-buffer."
  (condition-case nil
      ad-do-it
    (end-of-buffer)))

(global-set-key [(mouse-4)] 'fine-scroll-down)
(global-set-key [(mouse-5)] 'fine-scroll-up)
(global-set-key [(shift mouse-4)] 'coarse-scroll-down)
(global-set-key [(shift mouse-5)] 'coarse-scroll-up)

(defun my-scroll-down-command (event &optional lines)
  (with-selected-window (cl-caadr event)
    (condition-case nil
	(scroll-down lines)
      (beginning-of-buffer))))

(defun my-scroll-up-command (event &optional lines)
  (with-selected-window (cl-caadr event)
    (condition-case nil
	(scroll-up lines)
      (end-of-buffer))))

(defun fine-scroll-down (event)
  (interactive "e")
  (my-scroll-down-command event fine-scroll-lines))

(defun fine-scroll-up (event)
  (interactive "e")
  (my-scroll-up-command event fine-scroll-lines))

(defun coarse-scroll-down (event)
  (interactive "e")
  (my-scroll-down-command event))

(defun coarse-scroll-up (event)
  (interactive "e")
  (my-scroll-up-command event))

(provide 'intellimouse)
