.SUFFIXES: .el .elc

EMACS = emacs

.el.elc:
	$(EMACS) -batch -q -f batch-byte-compile $<

# Mine
ELCS =  extent.elc \
	itimer.elc \

# Others
ELCS += filladapt.elc \
	hide-copyleft.elc \
	igrep.elc \

all:	$(ELCS) $(EXTRA)

auto-autoloads.el: $(ELCS)
	rm -f auto-autoloads.el
	$(EMACS) -batch -q -no-site-file -l autoload -f batch-update-directory .

tags:
	etags *.el

clean:
	rm -f *.elc auto-autoloads.el TAGS
