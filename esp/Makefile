.SUFFIXES: .el .elc

EMACS = emacs

.el.elc:
	$(EMACS) -batch -q -f batch-byte-compile $<

# Mine
ELCS =  extent.elc \
	itimer.elc \
	missing.elc \

# Others
ELCS += filladapt.elc \
	hide-copyleft.elc \
	igrep.elc \

EXTRA = esp-loaddefs.elc

all:	$(ELCS) $(EXTRA)

esp-loaddefs.el: $(ELCS)
	@echo Create $(EMACS) esp-loaddefs.el ...
	@$(EMACS) -batch -q $(HELPER) -l build-loaddefs.el -f build-loaddefs

tags:
	etags *.el

clean:
	rm -f *.elc esp-loaddefs.el TAGS
