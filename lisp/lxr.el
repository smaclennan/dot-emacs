;;; lxr.el --- interface to lxr source browser

;; Copyright (C) 2000-2020 Sean MacLennan
;; Revision:   1.4

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

; 			      How it Works
;
; Basically, you need to setup two variables that link the lxr url to the
; local code base. `lxr-url' should point at the lxr repository and
; `lxr-base' should point at the local code base. Both of these variables
; are buffer local, since you (well let's face it I) want different
; repositories for different directories.
;
; Let's assume you only use lxr for the Linux kernel. One way to
; setup lxr would be to add it to your
; `c-mode-common-hook'. It would be overkill, but
; wouldn't hurt anything since you wouldn't use it outside the kernel.
;
; (defun lxr-setup ()
;   (setq lxr-url "https://elixir.bootlin.com/linux/"
;	lxr-version my-kernel-vers
; 	lxr-base (file-truename "/usr/src/linux-stable")))
;   (add-hook 'c-mode-common-hook 'lxr-setup)
;
; The `lxr-version' is used by repositories that have multiple versions.
; It will default to the most current version if left `nil'.
;
; 			      Requirements
;
;   * http.el to grab the lxr file from the url.
;   * h2t.el to convert the html to text.
;   * For GNU Emacs you need extent.el.
;   * And of course, lxr.el!
;
; 			     How to Use It
;
; There are currently two main interfaces: web output and definition
; files. The web output, `lxr', gives you a text based version of the lxr
; web output with clickable links to local files. When you click on a
; link, a tag is pushed so you can use `pop-tag-mark' to get back.
; `lxr-at-point' is similar except it grabs the word at the point.

; Definition files are more like the use in IDEs. With `lxr-defined' you
; are prompted for an identifier and then taken to the first file that
; matches (if any). `lxr-defined-at-point' is similar except it uses the
; word under the point.
;
; In both cases, `lxr-next-defined' can be used to go to the next file in
; the list (if any). Again tags are marked so you can pop back through the
; list of files.

;(let ((lxr-menu
;       `("----"
;	 ["lxr" lxr-at-point lxr-url]
;	 ["lxr defined" lxr-defined-at-point lxr-url]
;	 ["lxr next" lxr-next-defined lxr-url])))

;  (nconc c-c-menu lxr-menu)
;  (nconc c-c++-menu lxr-menu)
;  )
; )
; (add-hook 'c-initialization-hook 'my-c-initialization-hook)

(defvar lxr-url nil
  "*URL to use for lxr")
(make-variable-buffer-local 'lxr-url)

(defvar lxr-version nil
  "*Version string for lxr or nil for default")
(make-variable-buffer-local 'lxr-version)

(defvar lxr-base nil
  "*Base for lxr files")
(make-variable-buffer-local 'lxr-base)

(defvar lxr-defined-alist nil
  "Local variable with alist of files and line numbers.")

(defvar lxr-local-base nil "Local variable")

(define-minor-mode lxr-mode
  "Minor mode for LXR buffer."
  nil " LXR"
  '(([mouse-1] 'lxr-mousable)
    ([mouse-2] 'lxr-mousable)
    ("\C-m"    'lxr-keystroke)
    ("g"       'lxr-keystroke)))

(provide 'lxr)

(require 'h2t)
(require 'etags)


(defun http-get (url bufname callback)
  (url-retrieve url callback (list bufname)))

;;;###autoload
(defun lxr (ident)
  (interactive "sIdentifier: ")
  (unless lxr-url (setq lxr-url (read-string "lxr url: ")))
  (unless lxr-base (setq lxr-base (read-string "lxr base: ")))

  ;; https://elixir.bootlin.com/linux/v4.19.90/ident/WARN_ON
  (let ((url (concat (file-name-as-directory lxr-url)
		     (if lxr-version (concat "v" lxr-version) "current")
		     "/ident/" ident)))
    (url-retrieve url 'lxr-get-callback '("*lxr*")))
  (setq lxr-local-base (file-name-as-directory lxr-base))
  (pop-to-buffer "*lxr*"))

;;;###autoload
(defun lxr-at-point ()
  (interactive)
  (lxr (current-word)))

(defun lxr-test ()
  "For debugging"
  (interactive)
  (with-temp-buffer
   (insert-file-contents "~/tmp/lxr-output")
   (goto-char (point-min))
   (lxr-get-callback nil nil "*lxr*")
   ))

(defun lxr-get-callback (status buffer)
  ;; First make sure the http command succeeded
  (goto-char (point-min))
  (unless (looking-at "HTTP/[1-9]\\.[0-9]+ 200")
    ;; http error
    (error "http request failed: %s"
	   (buffer-substring (point-min) (progn (end-of-line) (point)))))

  ;; find the start of the lxr ident
  (search-forward "<div class=\"lxrident\">")
  (let ((start (point)))
    (search-forward "</div>")
    (copy-to-buffer buffer start (match-beginning 0)))

  (with-current-buffer buffer
    ;; Convert the page to text
    (set-buffer-modified-p nil)
    (html-to-text buffer lxr-mode-map t)
    (setq lxr-mode t)
    (set-buffer-modified-p nil))
  (message "Done"))

;; Find the extent nearest pos. Can return nil.
(defun lxr-nearest-extent (pos)
  (let ((extent (car (overlays-at pos))))
    (unless extent
      (setq extent (car (overlays-at (next-overlay-change pos))))
      (unless extent
	(setq extent (car (overlays-at (previous-overlay-change pos))))))
    extent))

(defun lxr-doit (pos)
  (let* ((extent (lxr-nearest-extent pos))
	 (anchor (overlay-get extent 'anchor))
	 line file)
    ;; v4.19.90/source/drivers/nvme/host/fc.c#L966
    (unless (string-match "source/\\([^#?]+\\)#L\\([0-9]+\\)" anchor)
      (error "Bad anchor %s" anchor))
    (setq file (concat lxr-local-base (match-string 1 anchor)))
    (setq line (string-to-number (match-string 2 anchor)))
    (xref-push-marker-stack)
    (find-file file)
    (goto-char 0)
    (forward-line (1- line))))

;; This is called on a mouse click
(defun lxr-mousable (event)
  (interactive "e")
  (lxr-doit (event-point event)))

;; This is called from a keystroke
(defun lxr-keystroke ()
  (interactive)
  (lxr-doit (point)))

