include ../Rules.mk

all:	$(ELCS) $(LOAD_FILES)

esp.elc: esp.el
	$(EMACS) -batch -q -f batch-byte-compile $<

clean:
	rm -f *.elc $(LOAD_FILES) TAGS
