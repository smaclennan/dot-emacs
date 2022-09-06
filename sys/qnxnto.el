(load "cpuid" nil noninteractive)
(load "unix"  nil noninteractive)

;; Currently the QNX libregex does not support \b
;; The stock git grep is compiled against libregex
(setq git-grep-full-regexp nil)

;; Does not work with git grep
(setq grep-use-null-device nil)

;; toybox grep does support \b, so use that
(setq my-grep-prog "toybox grep -nH")

(defun do-pidin-info ()
  (with-temp-buffer
    (call-process "pidin" nil t nil "info")
    (re-search-backward "Freemem:[0-9]+MB/\\([0-9]+\\)MB")
    (setq sys-mem (* (string-to-number (match-string 1)) #x100000))
    (setq sys-nproc (count-matches "^Processor"))))

(defun pidin-cpuinfo ()
  "Returns a list of model and arch."
  (with-temp-buffer
    (call-process "pidin" nil t nil "info")
    (goto-char (point-min))
    (let (model arch)
      (re-search-forward "^CPU: *\\([^ ]+\\)")
      (setq arch (match-string 1))
      (re-search-forward "^Processor1: [0-9]+ \\(.+\\)$")
      (setq model (match-string 1))
      (list model arch))))

;;;###autoload
(defun sys-nproc ()
  "Return the number of processors reported by pidin."
  (if sys-nproc sys-nproc (do-pidin-info)))

;; my-compile.el needs sys-nproc. Anything that required my-compile
;; failed at compile time. Calling sys-nproc here seems to solve it.
(sys-nproc)

;;;###autoload
(defun sys-mem ()
  "Return the total and free memory."
  (unless sys-mem (do-pidin-info))
  (list sys-mem
	;; Yes ls -ld /proc = free memory
	(nth 7 (file-attributes "/proc"))))

;;;###autoload
(defun sys-cpuinfo ()
  (if (executable-find "cpuid")
      (cpuid-cpuinfo)
    ;; Fall back to pidin
    (let ((pidin-info (pidin-cpuinfo)))
      (list (car pidin-info) (cadr pidin-info) 0 0 0))))
