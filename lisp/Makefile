include ../Rules.mk

DIRS := sam misc
ifneq ($(findstring xemacs,$(EMACS)),)
DIRS += xemacs
else
DIRS += emacs
endif

all:
	@for d in $(DIRS); do \
		$(MAKE) -C $$d $(MFLAGS) all; \
	done

clean:
	@for d in $(DIRS); do \
		$(MAKE) -C $$d $(MFLAGS) clean; \
	done
