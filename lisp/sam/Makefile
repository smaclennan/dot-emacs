include ../../Rules.mk

# Don't build windoze.el
ELCS := $(filter-out windoze.elc,$(ELCS))

all:	$(ELCS) $(LOAD_FILES)

tags:
	etags *.el

clean:
	rm -f *.elc $(LOAD_FILES) TAGS *~
