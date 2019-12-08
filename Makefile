all:
	@$(MAKE) -C lisp -s $(MFLAGS) all
	@$(MAKE) -C sys  -s $(MFLAGS) all
#	@$(MAKE) -C rc   -s $(MFLAGS) all

clean:
	find . -name "*.elc" -delete
	$(MAKE) -C lisp $(MFLAGS) clean
	@$(MAKE) -C sys -s $(MFLAGS) clean
	$(MAKE) -C rc   $(MFLAGS) clean
	$(MAKE) -C src  $(MFLAGS) clean
