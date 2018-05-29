
(if running-windoze
    ;; Different excludes for windows
    (setq smerge-diff-excludes
	  '("*.obj" "*.lib" "*.dll" "*.sbr" ".svn" "*.scc"
	    "*.plg" "*.pdb" "*.dep" "*.ncb" "*.opt"
	    "*.log" "*.wrn" "*.err"
	    "*.mak"
	    "objchk*" "objfre*"
	    "debug" "release" "Debug" "Release"))
  (setq smerge-diff-excludes
	'("*.o" "*.obj" "*.a" "*.lib" "*~" ".#*" "CVS" ".svn"
	  ".git" "*.cmd" "*.lo" "*.ko" ".tmp_versions" "*.Plo"
	  "modules.order" "*.elc" "*.mod.c" "TAGS" "*.builtin")))

(setq smerge-diff-options "-w")
