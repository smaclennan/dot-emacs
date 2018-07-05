.SUFFIXES: .el .elc

# When building inside of Emacs it seems EMACS=t
ifeq ($(EMACS),t)
EMACS=
endif

ifeq ($(EMACS),)
ifneq ($(findstring .xemacs, $(DIR)),)
EMACS=xemacs
else
ifneq ($(findstring .sxemacs, $(DIR)),)
EMACS=sxemacs
else
EMACS=emacs
endif
endif
endif
