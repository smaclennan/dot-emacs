.PHONY: all clean

LISP := $(wildcard *-rc.el)
ELCS = $(LISP:.el=.elc)

.el.elc:
	@echo Compile rc/$<
	@emacs -batch -q -l ~/.emacs.d/init \
		-eval "(would-like '`basename $< -rc.el`)" \
		-f batch-byte-compile $< \
		2>&1 | grep -v "^Loading" || true

all: $(ELCS)

clean:
	rm -f *.elc *~
