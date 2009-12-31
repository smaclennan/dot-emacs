BASE=$(shell basename $(PWD))
ifeq ($(BASE),.sxemacs)
EMACS := sxemacs
else ifeq ($(BASE),.xemacs)
EMACS := xemacs
else
EMACS := emacs
endif

all:
ifeq ($(BASE),.emacs.d)
	make EMACS=$(EMACS) -C esp
else
	make EMACS=$(EMACS) -C site-packages/lisp/introspector
endif
	make EMACS=$(EMACS) -C site-packages/lisp/sam
	make EMACS=$(EMACS) -C site-packages/lisp/anti-gnus

clean:
	rm -f *.elc
	make -C site-packages/lisp/sam clean
	make -C site-packages/lisp/introspector clean
	make -C site-packages/lisp/anti-gnus clean
	make -C esp clean
