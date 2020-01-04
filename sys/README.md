This directory is an attempt to put system and Emacs version specific
code in one place.

Currently we support:

OS      | system-type   | sys file
--------|---------------|-----------------
Linux   | gnu/linux     | linux.el
BSD     | berkeley-unix | berkeley-unix.el
QNX     | qnxnto        | qnxnto.el
Windows | windows-nt    | windows-nt.el

## Basic functions

* sys-os
  * os (distro for Linux) and release

* sys-nproc
  * number of cpus (Linux definition of cpu)
  * fills in sys-nproc variable

* sys-mem
  * total, free, and available memory
  * only Linux provides available memory
  * fills in sys-mem variable

* sys-cpuinfo
  * see below for output
  * BSD, QNX require cpuid installed

* sys-cpuflags
  * cpu flags
  * BSD, QNX, Windows require cpuid installed

* sys-is-guest
  * is the system a guest or virtual machine
  * BSD, QNX, Windows require cpuid installed

### sys-cpuinfo

I currently do not run Emacs on any non-x86 machines, so the output is
very x86-centric.

Sample x86_64 output: model_name, vendor, family, model, stepping

    ("Intel(R) Core(TM) i7-3630QM CPU @ 2.40GHz" "GenuineIntel" 6 58 9)

