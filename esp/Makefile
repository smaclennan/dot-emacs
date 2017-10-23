.SUFFIXES: .el .elc

EMACS = emacs

.el.elc:
	$(EMACS) -batch -q -f batch-byte-compile $<

# Mine
ELCS =  esp.elc extent.elc itimer.elc

# Others
ELCS += filladapt.elc igrep.elc rcfiles.elc hide-copyleft.elc

EXTRA = esp-loaddefs.elc

ifneq ($(wildcard mmm-mode/mmm-mode.el),)
# We can't compile bitbake without mmm-mode
ELCS += bitbake.elc
EXTRA += mmm
endif

all:	$(ELCS) $(EXTRA)

# Dummy... we already build elc files
elcs:

esp-loaddefs.el: $(ELCS)
	@echo Create $(EMACS) esp-loaddefs.el ...
	@$(EMACS) -batch -q $(HELPER) -l build-loaddefs.el -f build-loaddefs

mmm:	mmm-mode/Makefile
	$(MAKE) $(MFLAGS) -C mmm-mode

mmm-mode/Makefile:
	cd mmm-mode; ./autogen.sh; ./configure --with-emacs

tags:
	etags *.el

clean:
	rm -f *.elc esp-loaddefs.el TAGS
