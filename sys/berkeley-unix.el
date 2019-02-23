(defun sys-nproc () (string-to-number (shell-command-to-string "sysctl -n hw.ncpu")))
