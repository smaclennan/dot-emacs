.PHONY: all clean clean-sam

include ./Rules.mk

all:
	@echo $(MAKE) $(EMACS) ...
ifeq ($(EMACS),emacs)
	$(MAKE) EMACS=$(EMACS) -C esp
endif
	$(MAKE) EMACS=$(EMACS) -C site-packages/lisp

clean:
	@echo Clean $(EMACS) ...
	find -name "*.elc" -delete
	$(MAKE) -C site-packages/lisp clean
	$(MAKE) -C esp clean

clean-sam:
	$(MAKE) -C site-packages/lisp/sam clean
