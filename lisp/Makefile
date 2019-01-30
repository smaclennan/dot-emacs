include ../Rules.mk

# Don't build windoze.el
ELCS := $(filter-out windows-nt.elc,$(ELCS))

all:	$(ELCS) ../TAGS

../TAGS: $(LISP)
	@echo Building tags...
	@etags ../*.el ../lisp/*.el ../misc/*.el

clean:
	rm -f *.elc TAGS *~ $(CLEAN_EXTRA)
