include ../Rules.mk

all:	$(ELCS) ../TAGS

../TAGS: $(LISP)
	@echo Building tags...
	@etags ../*.el ../lisp/*.el ../misc/*.el

clean:
	rm -f *.elc TAGS *~ $(LOADFILE)
