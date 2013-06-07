### Makefile --- Generic lisp/ makefile for Emacs Lisp libraries

## Copyright (C) 2000-2013 Didier Verna

## Author: Didier Verna <didier@didierverna.net>

## This program is free software; you can redistribute it and/or modify
## it under the terms of the GNU General Public License version 3,
## as published by the Free Software Foundation.

## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.

## You should have received a copy of the GNU General Public License
## along with this program; if not, write to the Free Software
## Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.


### Commentary:

## Contents management by FCM version 0.1.

## Please use GNU make with this makefile.


### Code:

hack: all

include  ../Makefile.prj
include  ../Makefile.inc

-include ../local.prj
-include ../local.inc

include  Makefile.prj
-include local.inc

XEMACS      := $(shell if $(EMACS) --version \
			    | grep "XEmacs" > /dev/null; then		\
			 echo t;					\
		       fi)
XEMACS_21_5 := $(shell if $(EMACS) --version \
			    | grep "XEmacs 21.5" > /dev/null; then	\
			 echo t;					\
		       fi)

# #### NOTE: the lisp files are not detected automatically. They must be
# listed explicitely in Makefile.prj (which can, in turn detect them
# automatically), because the compilation order might be important (e.g. for
# getting macros). They are defined in $(EL_FILES).
ELC_FILES    := $(EL_FILES:%.el=%.elc)

ifeq ($(XEMACS),t)
AUTOLOAD_FILE := auto-autoloads.el
CUSLOAD_FILE  := custom-load.el
else
AUTOLOAD_FILE := $(PROJECT)-autoloads.el
CUSLOAD_FILE :=
endif

GEN_EL_FILES := $(AUTOLOAD_FILE) $(CUSLOAD_FILE)
ifeq ($(XEMACS_21_5),t)
GEN_EL_FILES +=  custom-defines.el
endif
ifeq ($(XEMACS),t)
GEN_ELC_FILES := $(GEN_EL_FILES:%.el=%.elc)
else
GEN_ELC_FILES :=
endif

ALL_EL_FILES  := $(EL_FILES)  $(GEN_EL_FILES)
ALL_ELC_FILES := $(ELC_FILES) $(GEN_ELC_FILES)

EL_STD_DIR       := $(PREFIX)/share/emacs/site-lisp
EL_PKG_DIR       := $(PKG_DIR)/lisp/$(PROJECT)

# For the compiler to find the require'd files:
ifeq ($(XEMACS),t)
ADD_MY_PATH := -eval '(push "." load-path)'
else
ADD_MY_PATH := --directory .
endif

#### FIXME: this is here because of Clox that needs to modify ELC_FILES, but
#### it shouldn't be.
-include Makefile.inc

all: gen-el-files all-elc-files

gen-el-files: $(GEN_EL_FILES)

all-elc-files: $(ALL_ELC_FILES)

install: all
	install -d -m 755 $(EL_STD_DIR)
ifeq ($(SYMLINK),)
	install -m 644 $(ALL_EL_FILES) $(ALL_ELC_FILES) $(EL_STD_DIR)/
else
	ln -fs $(abspath $(ALL_EL_FILES)) $(abspath $(ALL_ELC_FILES)) \
	  $(EL_STD_DIR)/
endif

uninstall:
	test -d $(EL_STD_DIR) \
	  && cd $(EL_STD_DIR) \
	  && rm -f $(ALL_ELC_FILES) $(ALL_EL_FILES)

install-pkg: all
	install -d -m 755 $(EL_PKG_DIR)
ifeq ($(SYMLINK),)
	install -m 644 $(ALL_EL_FILES) $(ALL_ELC_FILES) $(EL_PKG_DIR)/
else
	ln -fs $(abspath $(ALL_EL_FILES)) $(abspath $(ALL_ELC_FILES)) \
	  $(EL_PKG_DIR)/
endif

uninstall-pkg:
	test -d $(EL_PKG_DIR) && rm -fr $(EL_PKG_DIR)

clean:
	-rm *~

distclean: clean
	-rm -f $(ALL_ELC_FILES) $(GEN_EL_FILES)

$(AUTOLOAD_FILE): $(EL_FILES)
ifeq ($(XEMACS_21_5),t)
	$(EMACS) -batch -l autoload -f batch-update-directory-autoloads \
	  $(PROJECT) .
else ifeq ($(XEMACS),t)
	$(EMACS) -batch -q -l autoload				\
	  -eval '(setq autoload-package-name "$(PROJECT)")'	\
	  -f batch-update-directory .
else
	$(EMACS) --batch --load autoload	\
	  --eval "(let ((generated-autoload-file \
			 \"$(abspath $(AUTOLOAD_FILE))\")) \
		    (update-directory-autoloads \".\"))"
endif
	@rm -f $@~
	@touch $@

$(CUSLOAD_FILE): $(EL_FILES)
ifeq ($(XEMACS),t)
	$(EMACS) -batch -q -l cus-dep -f Custom-make-dependencies .
else
	$(EMACS) --batch --load cus-dep -f custom-make-dependencies .
endif
	@rm -f $@~
	@touch $@

ifeq ($(XEMACS_21_5),t)
custom-defines.el: $(EL_FILES)
	$(EMACS) -batch -l autoload -f batch-update-directory-custom-defines \
	  $(PROJECT) .
	@rm -f $@~
	@touch $@
endif


%.elc: %.el
ifeq ($(XEMACS),t)
	$(EMACS) -batch -q $(ADD_MY_PATH) -f batch-byte-compile $<
else
	$(EMACS) --batch $(ADD_MY_PATH) -f batch-byte-compile $<
endif

.PHONY: all								\
	elc-files all-elc-files						\
	install uninstall install-pkg uninstall-pkg clean distclean	\
	../Makefile.prj ../Makefile.inc Makefile.prj Makefile.inc	\
	../local.prj ../local.inc local.inc local.mak

-include local.mak

### Makefile ends here
