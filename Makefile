MFLAGS += -s

all:
	@$(MAKE) -C sys  $(MFLAGS) all
	@$(MAKE) -C lisp $(MFLAGS) all
#	@$(MAKE) -C rc   $(MFLAGS) all

clean:
	find . -name "*.elc" -delete
	$(MAKE) -C lisp $(MFLAGS) clean
	$(MAKE) -C sys  $(MFLAGS) clean
	$(MAKE) -C rc   $(MFLAGS) clean
	$(MAKE) -C src  $(MFLAGS) clean
