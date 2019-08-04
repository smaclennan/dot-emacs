EMACS = emacs
HELPER = -l ~/.emacs.d/lisp/batch-helper

all:
	$(EMACS) -batch -Q $(HELPER) -f update-sys

clean:
	rm -f *.elc sys-loaddefs.el sam-sys-loaddefs.el
