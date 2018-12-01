.PHONY: all clean

PREFIX ?= /usr/local

# If you set D=1 on the command line then $(D:1=-g) returns -g,
# else it returns the default (-O2).
D = -O2
CFLAGS += -Wall $(D:1=-g)

# If you set V=1 on the command line then you will get the actual
# commands displayed.
V	      = @
Q	      = $(V:1=)
QUIET_CC      = $(Q:@=@echo    '     CC       '$@;)
QUIET_RM      = $(Q:@=@echo    '     RM       '$@;)

.c.o:
	$(QUIET_CC)$(CC) -o $@ -c $(CFLAGS) $<

all: motley

motley: motley.c output.c lookup.c
	$(QUIET_CC)$(CC) -o $@ $(CFLAGS) $+

install: motley
	install -D motley $(DESTDIR)$(PREFIX)/bin/motley

clean:
	$(QUIET_RM)rm -f motley *.o
