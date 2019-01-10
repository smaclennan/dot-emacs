.PHONY: all clean

include ./Rules.mk

# I have had problems byte-compiling the rc directory
# Emacs will load the rc files multiple times

all:
	$(MAKE) -C lisp $(MFLAGS) all
	$(MAKE) -C misc $(MFLAGS) all
#	$(MAKE) -C rc   $(MFLAGS) all

clean:
	find . -name "*.elc" -delete
	$(MAKE) -C lisp $(MFLAGS) clean
	$(MAKE) -C misc $(MFLAGS) clean
	$(MAKE) -C rc   $(MFLAGS) clean
