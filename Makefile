.PHONY: all clean realclean

MFLAGS += -s

all:
	$(MAKE) -C lisp $(MFLAGS) all
	$(MAKE) -C sys  $(MFLAGS) all
	$(MAKE) -C rc   $(MFLAGS) all

clean:
	$(MAKE) -C lisp $(MFLAGS) clean
	$(MAKE) -C sys  $(MFLAGS) clean
	$(MAKE) -C rc   $(MFLAGS) clean
	rm -f user-lisp/.user-lisp-autoloads.el

realclean: clean
	$(MAKE) -C src  $(MFLAGS) clean
