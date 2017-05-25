.SUFFIXES: .el .elc

EMACS = emacs

.el.elc:
	$(EMACS) -batch -q -f batch-byte-compile $<

# Mine
ELCS =  esp.elc extent.elc itimer.elc

# Others
ELCS += filladapt.elc igrep.elc rcfiles.elc hide-copyleft.elc

EXTRA = esp-loaddefs.elc

all:	$(ELCS) $(EXTRA)

# Dummy... we already build elc files
elcs:

esp-loaddefs.el: $(ELCS)
	@echo Create $(EMACS) esp-loaddefs.el ...
	@$(EMACS) -batch -q $(HELPER) -l build-loaddefs.el -f build-loaddefs

tags:
	etags *.el

clean:
	rm -f *.elc esp-loaddefs.el TAGS
