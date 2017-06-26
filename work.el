(require 'my-compile)
(require 'my-tags)
(require 'cc-mode)

(add-to-list
 'copylefts-to-hide
 '("Copyright (c) [0-9]+ Diablo Technologies Inc\\." . "\\*/"))

;; Bart's Fault
(defun dmx-simple-func (matched-dir target &optional buf)
  ;; Linux style
  (kill-local-variable 'tab-width)
  (c-set-style "linux")
  (my-tag-dirs-helper matched-dir buf))

(defun bf-func (matched-dir target)
  (dmx-simple-func
   (replace-regexp-in-string "/barts-fault/.*" "/barts-fault/" matched-dir)
   target "*bf tags*"))

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
