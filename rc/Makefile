.PHONY: all clean

# We want the full init.el
HELPER := -l ~/.emacs.d/init

LISP := $(wildcard *-rc.el)
ELCS = $(LISP:.el=.elc)

.el.elc:
	@echo Compile $<
	@emacs -batch -q $(HELPER) \
		-eval "(require '`basename $< -rc.el`)" \
		-f batch-byte-compile $< \
		2>&1 | grep -v "^Loading" || true

all: $(ELCS) window-config.elc

window-config.elc: window-config.el
	@echo Compile $<
	@emacs -batch -q $(HELPER) -f batch-byte-compile $< \
		2>&1 | grep -v "^Loading" || true

clean:
	rm -f *.elc
