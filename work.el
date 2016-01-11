(require 'my-compile)

(add-to-list 'my-compile-dir-list
	     (list (expand-file-name "~/linux-kernel/") make-j "linux") t)

(add-to-list 'my-compile-dir-list
	     (list (expand-file-name "~/qemu/") make-j 'space-indent-4))
