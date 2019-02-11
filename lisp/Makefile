include ../Rules.mk

# Don't build windoze.el
ELCS := $(filter-out windows-nt.elc,$(ELCS))

all:	$(LOADFILE)

$(LOADFILE): $(ELCS)
	@echo "Update  $(LOADFILE)"
	@$(EMACS) -batch -Q $(HELPER) -f update-loadfile $(LOADFILE)

clean:
	rm -f *.elc TAGS *~ $(LOADFILE)
