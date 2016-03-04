(require 'my-compile)

(add-to-list 'my-compile-dir-list
	     (list (expand-file-name "~/linux-kernel/") make-j "linux") t)

(add-to-list 'my-compile-dir-list
	     (list (concat (expand-file-name "~") "/.*/drv/") make-j "linux") t)

;; Use the non-greedy match so include/qemu works
(add-to-list 'my-compile-dir-list
	     (list ".*?/qemu/" make-j 'space-indent-4) t)
