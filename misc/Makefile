include ../Rules.mk

all:	$(LOADFILE)

$(LOADFILE): $(ELCS)
	@echo "Update  $(LOADFILE)"
	@$(EMACS) -batch -q $(HELPER) -f update-loadfile $(LOADFILE)

clean:
	rm -f *.elc TAGS *~ $(LOADFILE)
