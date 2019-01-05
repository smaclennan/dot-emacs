;;; lxr.el --- interface to lxr source browser

;; Copyright (C) 2000-2009 Sean MacLennan
;; Revision:   1.3

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
;   (defun lxr-setup ()
;     (setq lxr-url "http://lxr.linux.no/"
; 	  lxr-version "2.6.26"
; 	  lxr-base (file-truename "/usr/src/linux")))
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

(defvar lxr-url nil
  "*URL to use for lxr")
(make-variable-buffer-local 'lxr-url)

(defvar lxr-version nil
  "*Version string for lxr or nil for default")
(make-variable-buffer-local 'lxr-version)

(defvar lxr-arch nil
  "*Architecture for lxr or nil for default.
You should never need to set this....")
(make-variable-buffer-local 'lxr-arch)

(defvar lxr-base nil
  "*Base for lxr files")
(make-variable-buffer-local 'lxr-base)

(defvar lxr-keymap nil
  "Keymap for lxr buffer.")

(defvar lxr-defined-alist nil
  "Local variable with alist of files and line numbers.")

;; SAM This forces lxr to be single threaded
(defvar lxr-local-base nil "Local variable")

;; Minor mode
(defvar lxr-mode-hook nil)
(defvar lxr-mode-on-hook nil)
(defvar lxr-mode-off-hook nil)

(define-minor-mode lxr-mode
  "Minor mode for LXR buffer."
  nil " LXR" lxr-keymap)
;; Minor mode

(provide 'lxr)

(require 'sam-common)
(require 'h2t)
(require 'etags)
(require 'trim)


(my-feature-cond
  (xemacs
   (require 'http)
   (defalias 'read-only-mode 'toggle-read-only))
  (emacs
   (defun http-get (url bufname callback)
     (url-retrieve url callback (list url bufname)))
   ))

(defun overlay-at (pos) (car (overlays-at pos)))

;;;###autoload
(defun lxr-at-point ()
  (interactive)
  (lxr (current-word)))

;;;###autoload
(defun lxr-defined (ident)
  (interactive "sIdentifier: ")
  (lxr ident 'lxr-defined-callback))

;;;###autoload
(defun lxr-defined-at-point ()
  (interactive)
  (lxr-defined (current-word)))

;;;###autoload
(defun lxr-next-defined ()
  (interactive)
  (unless lxr-defined-alist (error "No more files."))
  (let* ((list (pop lxr-defined-alist))
	 (file (nth 0 list))
	 (line (nth 1 list)))
    (push-tag-mark)
    (find-file file)
    (goto-char 0)
    (forward-line (1- line))))

(defun lxr-make-keymap ()
  (unless lxr-keymap
    (setq lxr-keymap (make-sparse-keymap "lxr"))
    (my-feature-cond
      (xemacs
       (define-key lxr-keymap 'button1 'lxr-mousable)
       (define-key lxr-keymap 'button2 'lxr-mousable))
      (emacs
       (define-key lxr-keymap [mouse-1] 'lxr-mousable)
       (define-key lxr-keymap [mouse-2] 'lxr-mousable)))
    (define-key lxr-keymap "\C-m" 'lxr-keystroke)
    (define-key lxr-keymap "g" 'lxr-keystroke)))

;;;###autoload
(defun lxr (ident &optional callback)
  (interactive "sIdentifier: ")
  (unless lxr-url (setq lxr-url (read-string "lxr url: ")))
  (unless lxr-base (setq lxr-base (read-string "lxr base: ")))
  (unless lxr-keymap (lxr-make-keymap))
  (unless callback (setq callback 'lxr-get-callback))
  (let ((buf (get-buffer "*lxr*"))
	(cur (current-buffer)))
    (when buf
      (set-buffer buf)
      (read-only-mode 0)
      (set-buffer cur)))
  (let ((url (concat lxr-url "/ident?"
		     (if lxr-version (concat "v=" lxr-version ";"))
		     (if lxr-arch (concat "a=" lxr-arch ";"))
		     "i=" ident)))
    (http-get url "*lxr*" callback))
  (setq lxr-local-base lxr-base) ;; lxr-base is buffer local
  (when (eq callback 'lxr-get-callback)
    (pop-to-buffer "*lxr*")))

(defun lxr-specific-cleanup ()
  (trim-lines)

  ;; Remove the trailer. Everything from "This page was" on.
  (when (re-search-forward "[ \t\n]*This page was automatically generated by" nil t)
    (goto-char (match-beginning 0)) (forward-char) ;; do not delete \n
    (kill-region (point) (point-max)))

  ;; Remove the header. The header is all the text from the start of
  ;; the buffer to the first word after "Cross Reference"
  (goto-char (point-min))
  (when (re-search-forward "Cross Reference[ \t\n]*" nil t)
    (kill-region (point-min) (match-end 0)))
  )

(defun lxr-get-callback (status cbargs)
  ;; First make sure the http command succeeded
  (goto-char (point-min))
  (unless (looking-at "HTTP/[1-9]\\.[0-9]+ 200")
    ;; http error
    (error "http request failed: %s"
	   (buffer-substring (point-min) (progn (end-of-line) (point)))))

  ;; SAM fixme file-name-as-directory?
  (unless (string-match "/$" lxr-local-base)
    (setq lxr-local-base (concat lxr-local-base "/")))

  ;; stip the server header
  (search-forward-regexp "^[ \t\r]*\n" nil t)

  (let ((buffer (cadr cbargs)))
    (copy-to-buffer buffer (point) (point-max))

    (with-current-buffer buffer
      ;; Convert the page to text
      (set-buffer-modified-p nil)
      (html-to-text buffer lxr-keymap t)
      (lxr-specific-cleanup)
      (setq lxr-mode t)
      (set-buffer-modified-p nil)
      (read-only-mode 1)
      ))
  (message "Done"))

(defun lxr-defined-callback (status cbargs)
  ;; First make sure the http command succeeded
  (goto-char (point-min))
  (unless (looking-at "HTTP/[1-9]\\.[0-9]+ 200")
    ;; http error
    (error "http request failed: %s"
	   (buffer-substring (point-min) (progn (end-of-line) (point)))))

  ;; SAM fixme file-name-as-directory?
  (unless (string-match "/$" lxr-local-base)
    (setq lxr-local-base (concat lxr-local-base "/")))


  (let ((buffer (cadr cbargs)) start)
    ;; Isolate the "Defined as" list.
    (unless (re-search-forward "Defined as a [a-z ]+ in:<ul>" nil t)
      (error "Not defined."))
    (setq start (match-end 0))

    (unless (re-search-forward "</ul>")
      (error "Missing </ul>"))

    (copy-to-buffer buffer start (match-beginning 0))
    (with-current-buffer buffer
      ;; Now find all the files.
      (let (file line count)
	(setq lxr-defined-alist nil) ;; clear out old list
	(goto-char (point-min))
	(while (re-search-forward "<li>" nil t)
	  ;; Fail if we don't match - I want to catch misformed lines
	  (re-search-forward "<a href=[^>]+>\\([^,]+\\), line \\([0-9]+\\)</a>")
	  (setq file (concat lxr-local-base (match-string 1))
		line (string-to-number (match-string 2)))
	  (push (list file line) lxr-defined-alist))
	(setq count (length lxr-defined-alist))
	(if (eq count 1)
	    (message "Found file.")
	  (message "Found %d files." count)))
      )
    (lxr-next-defined)))

;; Find the extent nearest pos. Can return nil.
(defun lxr-nearest-extent (pos)
  (let ((extent (overlay-at pos)))
    (unless extent
      (setq extent (overlay-at (next-overlay-change pos)))
      (unless extent
	(setq extent (overlay-at (previous-overlay-change pos)))
	))
    extent))

(defun lxr-doit (pos)
  (let* ((extent (lxr-nearest-extent pos))
	 (anchor (overlay-get extent 'anchor))
	 line file)
    (cond
     ;; source file
     ;; source/kernel/daytona/daytona.c?v=git-monza#L172
     ((string-match (concat "^source/"
			    "\\([^#?]+\\)"
			    "\\(\\?v=[^#?]+\\)?"
			    "\\(\\?a=[^#?]+\\)?"
			    "#L\\([0-9]+\\)")
		    anchor)
      (setq line (string-to-number (match-string 4 anchor)))
      (setq file (concat lxr-local-base (match-string 1 anchor)))
      (push-tag-mark)
      (find-file file)
      (goto-char 0)
      (forward-line (1- line)))

     ;; ident (e.g. class reference)
     ((string-match "^ident" anchor)
      (http-get (concat lxr-url "/" anchor) "*lxr*" 'lxr-get-callback)
      (pop-to-buffer "*lxr*"))

     ;; generic http (e.g. lxr reference in trailer)
     ((string-match "^http:.*" anchor)
      (browse-url (match-string 0 anchor)))

     ;; huh?
     (t (error "Unknown anchor '%S'" anchor)))
    ))

;; This is called on a mouse click
(defun lxr-mousable (event)
  (interactive "e")
  (lxr-doit (event-point event)))

;; This is called from a keystroke
(defun lxr-keystroke ()
  (interactive)
  (lxr-doit (point)))

