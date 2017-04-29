(require 'my-compile)
(require 'my-tags)

;; Bart's Fault
(defvar bf-app-dirs '("barf" "bfmgr" "dmxmgr" "lib")
  "* List of application sub-directories.")

(defun bf-func (matched-dir target)
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

    (my-tag-dirs-helper
     (replace-regexp-in-string "/barts-fault/.*" "/barts-fault/" matched-dir)
     "*bf tags*")
    ))

(defun dmx-simple-func (matched-dir target)
      ;; Linux style
      (kill-local-variable 'tab-width)
      (c-set-style "linux")

      (add-hook 'after-save-hook 'my-tag-simple))

(add-to-list 'my-compile-dir-list (list ".*?/barts-fault/drv/" make-j 'bf-func) t)
(add-to-list 'my-compile-dir-list (list ".*?/barts-fault/dmx-trap/" make-j 'dmx-simple-func) t)
(add-to-list 'my-compile-dir-list (list ".*?/barts-fault/guest/drv/" make-j 'dmx-simple-func) t)
(add-to-list 'my-compile-dir-list '(".*?/barts-fault/" nil bf-func) t)

;; 2.x kernel
(add-to-list 'my-compile-dir-list
	     (list (expand-file-name "~/linux-kernel/") make-j "linux") t)

;; qemu
(defun qemu-func (matched-dir target)
  (space-indent-4 matched-dir target)
  (my-tag-dirs-helper matched-dir "*qemu tags*"))

;; Use the non-greedy match so include/qemu works
(add-to-list 'my-compile-dir-list
	     (list ".*?/qemu/" make-j 'qemu-func) t)

