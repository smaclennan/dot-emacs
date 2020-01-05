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
  (goto-char (point-min))
  (re-search-forward (concat "^" field "[ \t]+: \\(.*\\)$"))
  (match-string 1))

(defun cpuid-cpuinfo ()
  (let ((exe (cpuid-cpuinfo-exe)))
    (with-temp-buffer
      (call-process exe nil t nil)
      (list
       (cpuid-find "Model Name")
       (cpuid-find "Vendor")
       (string-to-number (cpuid-find "Family"))
       (string-to-number (cpuid-find "Model"))
       (string-to-number (cpuid-find "Stepping"))))))

(defun cpuid-cpu-flags ()
  "This is a subset of the flags on Linux."
  (let ((exe (cpuid-cpuinfo-exe)))
    (with-temp-buffer
      (call-process exe nil t nil)
      (split-string (cpuid-find "Flags")))))
