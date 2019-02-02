# I have had problems byte-compiling the rc directory
# Emacs will load the rc files multiple times

all:
	@$(MAKE) -C lisp PDIR=lisp/ -s $(MFLAGS) all
	@$(MAKE) -C misc PDIR=misc/ -s $(MFLAGS) all
#	@$(MAKE) -C rc   PDIR=rc/   -s $(MFLAGS) all

clean:
	find . -name "*.elc" -delete
	$(MAKE) -C lisp $(MFLAGS) clean
	$(MAKE) -C misc $(MFLAGS) clean
	$(MAKE) -C rc   $(MFLAGS) clean
