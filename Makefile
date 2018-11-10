.PHONY: all clean

include ./Rules.mk

all:
	@echo $(MAKE) $(EMACS) ...
	@$(MAKE) EMACS=$(EMACS) -C lisp all

clean:
	@echo Clean $(EMACS) ...
	find . -name "*.elc" -delete
	$(MAKE) -C lisp clean

