;; -*- lexical-binding: t -*-
;;; ogrok.el --- Emacs interface to OpenGrok source browser
;; Copyright (C) 2017 Sean MacLennan
;; Revision:   0.3

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

;; This is a very crude interface to OpenGrok.

(defgroup ogrok nil "opengrok variables" :group 'tools)

(defcustom ogrok-url nil "*URL to use for opengrok" :type 'string)

(defcustom ogrok-project nil "*Project to use with opengrok. Must be set." :type 'string)

(defcustom ogrok-xref "/xref/" "*opengrok xref prefix." :type 'string)

(defcustom ogrok-path nil "*Optional path argument to opengrok." :type 'string)

(defcustom ogrok-base nil "*Local base for opengrok files" :type 'string)

(defcustom ogrok-smart nil "*Try to be smart about matching rather than precise." :type 'boolean)

(provide 'ogrok)

(require 'compile)
(require 'etags)


(defun ogrok-compilation-parse ()
  ;; I tried to use compilation but it only worked 90% of the time.
  (setq buffer-read-only nil)
  (compilation--parse-region (point-min) (point-max))
  (setq buffer-read-only t)
  (goto-char (point-min)))

(defun ogrok-common (defs refs)
  (unless ogrok-url (setq ogrok-url (read-string "url: ")))
  (unless ogrok-project (setq ogrok-project (read-string "project: ")))
  (unless ogrok-base (setq ogrok-base (read-string "local base: ")))
  (xref-push-marker-stack)
  (ogrok-get-page
   (concat ogrok-url "/search?q=&defs=" defs "&refs=" refs
	   "&path=" ogrok-path
	   "&hist=&type=&project=" ogrok-project))
  (let ((ogrok-list (ogrok-parse-page)))
    (if (and ogrok-smart (eq (length ogrok-list) 1))
	(ogrok-goto (car ogrok-list))
      (display-buffer "*ogrok*" '(nil (window-height . 16))))))

(defun ogrok-get-ident (prompt)
  (if (or current-prefix-arg (not (looking-at "[a-zA-Z0-9_]")))
      (read-string prompt)
    (current-word)))

;;;###autoload
(defun ogrok-defs (&optional ident)
  (interactive)
  (unless ident
    (setq ident (ogrok-get-ident "Def: ")))
  (ogrok-common ident nil))

;;;###autoload
(defun ogrok-refs (&optional ident)
  (interactive)
  (unless ident
    (setq ident (ogrok-get-ident "Refs: ")))
  (ogrok-common nil ident))

(defun ogrok-goto (this)
  (let ((filename (concat ogrok-base "/" (nth 0 this)))
	(line (string-to-number (nth 1 this))))
    (find-file filename)
    (with-no-warnings (goto-line line))))

;; We want this synchronous so we can make decisions based on the results
(defun ogrok-get-page (url)
  (let ((buf (url-retrieve-synchronously url t)))
    (unless buf (error "get failed: %s" url))
    (with-current-buffer buf
      (when (get-buffer "*ogrok*") (kill-buffer "*ogrok*"))
      (rename-buffer "*ogrok*"))))

(defun ogrok-parse-page ()
  (let (ogrok-list more this regex)
    (with-current-buffer "*ogrok*"
      (goto-char (point-min))
      ;; Isolate the <div id="results">
      (when (search-forward "<div id=\"results\">" nil t)
	(delete-region (point-min) (point)))
      (when (search-forward "</div>" nil t)
	(delete-region (point) (point-max)))
      ;; More?
      (goto-char (point-min))
      (when (search-forward "class=\"more\"" nil t)
	(setq more t))

      ;; Get all the hrefs
      (goto-char (point-min))
      (setq regex (concat "class=\"s\" href=\"" ogrok-xref "\\([^#]+\\)#\\([0-9]+\\)\">"))
      (while (re-search-forward regex nil t)
	(setq this (list (match-string 1) (match-string 2)))
	(when (looking-at "<span class=\"l\">[0-9]+</span>")
	  (goto-char (match-end 0)))
	(when (re-search-forward ".+?<br/>" nil t) ;; non-greedy match
	  (nconc this (list (ogrok-strip-tags (match-string 0)))))
	(if ogrok-smart
	    (when (string-match "\\(function\\|macro\\) *$" (nth 2 this))
	      (setq ogrok-list (nconc ogrok-list (list this))))
	  (setq ogrok-list (nconc ogrok-list (list this)))))

      ;; Fixup the buffer and parse it
      (erase-buffer)
      (insert (concat "-*- mode: compilation; default-directory: \""
		      (abbreviate-file-name ogrok-base) "\" -*-\n"))
      (dolist (file ogrok-list)
	(insert (format "%s:%s:1:" (car file) (cadr file)))
	(when (eq (length file) 3) (insert (concat " " (nth 2 file))))
	(insert "\n"))
      (when more (insert "\nMore...\n"))
      (compilation-mode "ogrok")
      (setq default-directory ogrok-base)
      (ogrok-compilation-parse))
    ogrok-list))

(defun ogrok-strip-tags (line)
  (while (string-match "<[^>]+>" line)
    (setq line (replace-match "" nil nil line)))
  (when (string-match "^[ \t]+" line)
    (setq line (replace-match "" nil nil line)))
  (while (string-match "&amp;" line)
    (setq line (replace-match "&" nil nil line)))
  line)

;; For debugging
;; mutex_init (multiple)
;; in_init (single)
(when nil
  (setq ogrok-url "https://nxr.netbsd.org"
	ogrok-project "src"
	ogrok-base (expand-file-name "~/tmp/netbsd/")
	ogrok-xref "/xref/"
	ogrok-path nil)
  )

(when nil
  (setq ogrok-url "http://localhost:8080"
	ogrok-project "src"
	ogrok-base (expand-file-name "~/tmp/netbsd/")
	ogrok-xref "/xref/"
	ogrok-path nil)
  )
