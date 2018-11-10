include ../../Rules.mk

# Mine
ELCS =  esp.elc extent.elc

# Others
ELCS += rcfiles.elc hide-copyleft.elc

all:	$(ELCS) $(LOAD_FILES)

esp.elc: esp.el
	$(EMACS) -batch -q -f batch-byte-compile $<

clean:
	rm -f *.elc $(LOAD_FILES) TAGS
