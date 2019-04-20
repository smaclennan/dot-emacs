.PHONY: all clean

LISP := $(wildcard *-rc.el)
ELCS = $(LISP:.el=.elc)

.el.elc:
	@echo Compile rc/$<
	@emacs -batch -q -l ~/.emacs.d/lisp/batch-helper \
		-l ~/.emacs.d/init \
		-eval "(would-like '`basename $< -rc.el`)" \
		-f batch-byte-compile $< \
		2>&1 | grep -v "^Loading" || true

all: $(ELCS) window-config.elc

window-config.elc: window-config.el
	@echo Compile rc/$<
	@emacs -batch -q -l ~/.emacs.d/init -f batch-byte-compile $< \
		2>&1 | grep -v "^Loading" || true

clean:
	rm -f *.elc *~
