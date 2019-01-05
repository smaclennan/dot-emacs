.PHONY: all clean

# We want the full init.el
HELPER := -l ~/.emacs.d/init

LISP := $(wildcard *-rc.el)
ELCS = $(LISP:.el=.elc)

.el.elc:
	@echo Compile $<
	@echo "(require '`basename $< -rc.el`)" > build.sh
	@emacs -batch -q $(HELPER) -l build.sh -f batch-byte-compile $< > build.out 2>&1
	@grep -v "^Loading" build.out || true

all: $(ELCS) window-config.elc

window-config.elc: window-config.el
	@emacs -batch -q $(HELPER) -f batch-byte-compile window-config.el  > build.out 2>&1
	@grep -v "^Loading" build.out || true
	@rm -f build.sh build.out

clean:
	rm -f *.elc build.sh build.out
