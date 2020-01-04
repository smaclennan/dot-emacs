This directory is an attempt to put system and Emacs version specific
code in one place.

Currently we support:

OS      | sys file
--------|---------
Linux   | linux.el
BSD     | berkeley-unix.el
QNX     | qnxnto.el
Windows | windows-nt.el

## Basic functions

* sys-os
  * os (distro for Linux) and release

* sys-nproc
  * fills in sys-nproc variable

* sys-mem
  * fills in sys-mem variable

* sys-cpuinfo
  * BSD, QNX require cpuid installed

* sys-cpuflags
* sys-is-guest
  * BSD, QNX, Windows require cpuid installed
