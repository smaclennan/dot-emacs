all:
	@$(MAKE) -C lisp -s $(MFLAGS) all
	@$(MAKE) -C misc -s $(MFLAGS) all
#	@$(MAKE) -C rc   -s $(MFLAGS) all

clean:
	find . -name "*.elc" -delete
	$(MAKE) -C lisp $(MFLAGS) clean
	$(MAKE) -C misc $(MFLAGS) clean
	$(MAKE) -C rc   $(MFLAGS) clean
	$(MAKE) -C src  $(MFLAGS) clean
