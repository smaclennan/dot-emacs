(defvar cpuid-exe nil "The location of cpuid.")

(defun cpuid-cpuinfo-exe ()
  (unless cpuid-exe
    (setq cpuid-exe (concat user-emacs-directory "src/cpuid"))
    (unless (file-exists-p cpuid-exe)
      (setq cpuid-exe (executable-find "cpuid"))
      (unless cpuid-exe
	(if (eq sys-arch 'x86)
	    (error (concat "Maybe build " user-emacs-directory "src/cpuid?"))
	  (error "Not supported")))))
  cpuid-exe)

(defun cpuid-find (field)
  (re-search-forward (concat "^" field "[ \t]+: \\(.*\\)$"))
  (match-string 1))

(defun cpuid-cpuinfo ()
  (with-temp-buffer
    (call-process (cpuid-cpuinfo-exe) nil t)
    (goto-char (point-min))
    (list (cpuid-find "Model Name")
	  (cpuid-find "Vendor")
	  (string-to-number (cpuid-find "Family"))
	  (string-to-number (cpuid-find "Model"))
	  (string-to-number (cpuid-find "Stepping")))))

(defun cpuid-cpu-flags ()
  "This is a subset of the flags on Linux."
  (with-temp-buffer
    (call-process (cpuid-cpuinfo-exe) nil t)
    (goto-char (point-min))
    (split-string (cpuid-find "Flags"))))
