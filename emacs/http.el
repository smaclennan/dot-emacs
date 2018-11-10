;;; http.el --- Get http pages from the net

;; Copyright (C) 2018 Sean MacLennan
;; Revision:   1.0
;; GNU Emacs only

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

(defun http-default-callback (status cbargs)
  "The default http/https callback."
  (and status
       (plist-get status ':error)
       (error "Failed to retrieve url %s" (car cbargs)))
  (copy-to-buffer (cadr cbargs) (point-min) (point-max)))

;;;###autoload
(defun http-get (url &optional bufname callback)
  "Get an http or https URL asynchronously. If BUFNAME is nil,
use '*http get page*'. If CALLBACK is nil, use
`http-default-callback'. The callback is passed two args: the
first arg is a status, the second a list of (url bufname)."
  (interactive "sURL: ")
  (unless bufname  (setq bufname "*http get page*"))
  (unless callback (setq callback 'http-default-callback))
  (url-retrieve url callback (list url bufname)))

(provide 'http)
