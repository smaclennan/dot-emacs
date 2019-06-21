(defun cpuid-cpuinfo-exe ()
  (let ((exe (executable-find "cpuid")))
    (unless exe
      (if (string-match "x86" (uname "-m"))
	  (error (concat "Not supported. Maybe install "
			 user-emacs-directory "src/cpuid."))
	(error "Not supported")))
    exe))

(defun cpuinfo-find (field)
  "Find a field in cpuinfo output."
  (goto-char (point-min))
  (re-search-forward (concat "^" field "[ \t]+: \\(.*\\)$"))
  (match-string 1))

(defun cpuid-cpuinfo ()
  (let ((exe (cpuid-cpuinfo-exe)))
    (with-temp-buffer
      (shell-command exe t)
      (list
       (cpuinfo-find "Model Name")
       (cpuinfo-find "Vendor")
       (string-to-number (cpuinfo-find "Family"))
       (string-to-number (cpuinfo-find "Model"))
       (string-to-number (cpuinfo-find "Stepping"))))))

(defun cpuid-cpu-flags ()
  "This is a subset of the flags on Linux."
  (let ((exe (cpuid-cpuinfo-exe)))
    (with-temp-buffer
      (shell-command exe t)
      (cpuinfo-find "Flags"))))
