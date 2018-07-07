include ../Rules.mk

# Mine
ELCS =  esp.elc extent.elc itimer.elc

# Others
ELCS += filladapt.elc igrep.elc rcfiles.elc hide-copyleft.elc

EXTRA = esp-loaddefs.el

all:	$(ELCS) $(EXTRA)

esp.elc: esp.el
	$(EMACS) -batch -q -f batch-byte-compile $<

clean:
	rm -f *.elc esp-loaddefs.el TAGS
