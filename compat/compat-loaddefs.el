;;; compat-loaddefs.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads nil "xref-compat" "xref-compat.el" (23649 51257
;;;;;;  228463 43000))
;;; Generated autoloads from xref-compat.el

(autoload 'xref-find-definitions "xref-compat" "\
Find tag (in current tags table) whose name contains IDENTIFIER.

This is not a correct implementation of xref-find-definitions. If
there are multiple definitions it always goes to the most exact
definition. I provide a `find-tag-next' to go to the next
definition.

\(fn IDENTIFIER)" t nil)

(autoload 'find-tag-next "xref-compat" "\


\(fn)" t nil)

(autoload 'xref-find-references "xref-compat" "\


\(fn INDENTIFIER)" t nil)

(autoload 'xref-push-marker-stack "xref-compat" "\


\(fn)" t nil)

;;;***

(provide 'compat-loaddefs)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; compat-loaddefs.el ends here
