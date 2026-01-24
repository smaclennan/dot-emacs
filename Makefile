MFLAGS += -s

all:
	@$(MAKE) -C sys  $(MFLAGS) all
	@$(MAKE) -C lisp $(MFLAGS) all
#	@$(MAKE) -C rc   $(MFLAGS) all

# Even if you don't want to compile everything, you still must
# generate the loadfiles
loadfiles:
	@$(MAKE) -C sys  $(MFLAGS) all
	@$(MAKE) -C lisp $(MFLAGS) loadfile

clean:
	find . -name "*.elc" -delete
	$(MAKE) -C lisp $(MFLAGS) clean
	$(MAKE) -C sys  $(MFLAGS) clean
	$(MAKE) -C rc   $(MFLAGS) clean
	$(MAKE) loadfiles

realclean: clean
	$(MAKE) -C src  $(MFLAGS) clean
