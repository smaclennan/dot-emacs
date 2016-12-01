(require 'my-compile)

(add-to-list 'my-compile-dir-list
	     (list (expand-file-name "~/linux-kernel/") make-j "linux") t)

;; Use the non-greedy match so include/qemu works
(add-to-list 'my-compile-dir-list
	     (list ".*?/qemu/" make-j 'space-indent-4) t)

(defvar bf-dir nil "Internal variable.")
(make-variable-buffer-local 'bf-dir)

(defvar bf-app-dirs '("barf" "bfmgr" "dmxmgr")
  "* List of application sub-directories.")

(defun bf-tags ()
  (when bf-dir
    (let ((buf (get-buffer-create "*bf tags *")))
      ;; Emacs does not have (erase-buffer buf) :(
      (save-current-buffer (set-buffer buf) (erase-buffer))
      (my-tag-tree bf-dir buf))))

;; Use one global tagfile
(defun bf-tag-file (matched-dir)
  (setq bf-dir
	(replace-in-string matched-dir "/barts-fault/.*" "/barts-fault/"))

  (make-local-variable 'tags-file-name)
  (setq tags-file-name (concat bf-dir "TAGS"))

  (add-hook 'after-save-hook 'bf-tags))

(defun bf-func (matched-dir target)
  (require 'etags)

  (let ((dir (buffer-file-name)) subdir)
    ;; Get the subdir under bart's fault
    (when (string-match "/barts-fault/\\([^/]+\\)/" dir)
      (setq subdir (substring dir (match-beginning 1) (match-end 1))))

    ;; Set driver directories to Linux style and apps to spaces
    (if (member subdir bf-app-dirs)
	(setq c-basic-offset 4 tab-width 4 indent-tabs-mode nil)
      ;; I set tab-width in my-c-mode-common-hook. Reset it here.
      (kill-local-variable 'tab-width)
      (c-set-style "linux"))

    (bf-tag-file matched-dir)
    ))

(defun bf-guest-func (matched-dir target)
  (require 'etags)

  (if (string-match "/drv/" matched-dir)
      (progn
	;; Set driver directories to Linux style and apps to spaces
	;; I set tab-width in my-c-mode-common-hook. Reset it here.
	(kill-local-variable 'tab-width)
	(c-set-style "linux"))
    (setq c-basic-offset 4 tab-width 4 indent-tabs-mode nil))

  (add-hook 'after-save-hook 'my-tag-simple))

;; Bart's Fault
(add-to-list 'my-compile-dir-list (list ".*?/barts-fault/drv/" make-j 'bf-func) t)
(add-to-list 'my-compile-dir-list (list ".*?/barts-fault/guest/" nil 'bf-guest-func) t)
(add-to-list 'my-compile-dir-list '(".*?/barts-fault/" nil bf-func) t)

(add-to-list 'my-compile-dir-list '(".*?/drv/" make-j "linux") t)
