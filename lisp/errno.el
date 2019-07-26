(defvar errno-strings
  '("SUCCESS"
    "EPERM"
    "ENOENT"
    "ESRCH"
    "EINTR"
    "EIO"
    "ENXIO"
    "E2BIG"
    "ENOEXEC"
    "EBADF"
    "ECHILD"
    "EAGAIN"
    "ENOMEM"
    "EACCES"
    "EFAULT"
    "ENOTBLK"
    "EBUSY"
    "EEXIST"
    "EXDEV"
    "ENODEV"
    "ENOTDIR"
    "EISDIR"
    "EINVAL"
    "ENFILE"
    "EMFILE"
    "ENOTTY"
    "ETXTBSY"
    "EFBIG"
    "ENOSPC"
    "ESPIPE"
    "EROFS"
    "EMLINK"
    "EPIPE"
    "EDOM"
    "ERANGE"

    "EDEADLK"
    "ENAMETOOLONG"
    "ENOLCK"
    "ENOSYS"
    "ENOTEMPTY"
    "ELOOP"
    nil
    "ENOMSG"
    "EIDRM"
    "ECHRNG"
    "EL2NSYNC"
    "EL3HLT"
    "EL3RST"
    "ELNRNG"
    "EUNATCH"
    "ENOCSI"
    "EL2HLT"
    "EBADE"
    "EBADR"
    "EXFULL"
    "ENOANO"
    "EBADRQC"
    "EBADSLT"
    nil
    "EBFONT"
    "ENOSTR"
    "ENODATA"
    "ETIME"
    "ENOSR"
    "ENONET"
    "ENOPKG"
    "EREMOTE"
    "ENOLINK"
    "EADV"
    "ESRMNT"
    "ECOMM"
    "EPROTO"
    "EMULTIHOP"
    "EDOTDOT"
    "EBADMSG"
    "EOVERFLOW"
    "ENOTUNIQ"
    "EBADFD"
    "EREMCHG"
    "ELIBACC"
    "ELIBBAD"
    "ELIBSCN"
    "ELIBMAX"
    "ELIBEXEC"
    "EILSEQ"
    "ERESTART"
    "ESTRPIPE"
    "EUSERS"
    "ENOTSOCK"
    "EDESTADDRREQ"
    "EMSGSIZE"
    "EPROTOTYPE"
    "ENOPROTOOPT"
    "EPROTONOSUPPORT"
    "ESOCKTNOSUPPORT"
    "EOPNOTSUPP"
    "EPFNOSUPPORT"
    "EAFNOSUPPORT"
    "EADDRINUSE"
    "EADDRNOTAVAIL"
    "ENETDOWN"
    "ENETUNREACH"
    "ENETRESET"
    "ECONNABORTED"
    "ECONNRESET"
    "ENOBUFS"
    "EISCONN"
    "ENOTCONN"
    "ESHUTDOWN"
    "ETOOMANYREFS"
    "ETIMEDOUT"
    "ECONNREFUSED"
    "EHOSTDOWN"
    "EHOSTUNREACH"
    "EALREADY"
    "EINPROGRESS"
    "ESTALE"
    "EUCLEAN"
    "ENOTNAM"
    "ENAVAIL"
    "EISNAM"
    "EREMOTEIO"
    "EDQUOT"
    "ENOMEDIUM"
    "EMEDIUMTYPE"
    "ECANCELED"
    "ENOKEY"
    "EKEYEXPIRED"
    "EKEYREVOKED"
    "EKEYREJECTED"
    "EOWNERDEAD"
    "ENOTRECOVERABLE"
    "ERFKILL"
    "EHWPOISON"
    ))

;;;###autoload
(defun errno-string (errno)
  "Lookup ERRNO in `errno-strings'.
By default ERRNO should be a number. With a prefix arg ERRNO
should be a string."
  (interactive
   (list (if current-prefix-arg
	     (read-string "error: ")
	   (read-number "errno: "))))
  (let (str pos)
    (if (numberp errno)
	(setq str (nth errno errno-strings)
	      pos errno)
      (setq str (upcase errno))
      (when (equal str "EWOULDBLOCK") (setq str "EAGAIN"))
      (setq pos (cl-position str errno-strings :test 'equal)))
    (if (and str pos)
	(message "%s %d" str pos)
      (error "Not found"))))

(provide 'errno)
