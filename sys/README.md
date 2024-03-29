# System Specific Information

This directory is an attempt to put system and Emacs version specific
code in one place.

We try to support GNU Emacs >= 24. The basic strategy is to keep up to
date with the latest version and backport changes to previous
releases. We cannot guarantee that previous releases work fully.

## Supported Systems

Linux: Is fully supported since I use Emacs on it every day. ARM Linux
       is experimental.

QNX: Is supported, but I don't use it as often.

BSD: I very rarely use any more. FreeBSD probably has the best
     support.

Windows: My laptop died years ago. 'Nuff said.

Mac: I have never run Emacs on a Mac. The darwin.el code is
     experimental.

## Basic variables

* sys-arch
  * always set
* sys-mem
  * filled in by sys-mem()
  * filled in by sys-nproc() on QNX
* sys-nproc
  * filled in by sys-nproc()
  * filled in by sys-mem() on QNX

## Basic functions

* sys-os
  * os and release
* sys-linux-distro
  * Linux distro
* sys-nproc
  * number of cpus (Linux definition of cpu)
* sys-mem
  * total, free, and available memory
  * only Linux provides available memory
* sys-cpuinfo
  * see below for output
  * BSD, QNX require cpuid installed
* sys-cpuflags
  * cpu flags
  * BSD, QNX, Windows require cpuid installed
* sys-is-guest
  * is the system a guest or virtual machine
  * does not work for arm Linux
  * BSD, QNX, Windows require cpuid installed
* sysctl
  * return sysctl arg as a number
  * not supported on Windows

### sys-info

Basic system info in human readable form.

Sample Linux output:

    Linux 5.15.63
    Ubuntu 20.04.5 LTS (Focal Fossa)
    Intel(R) Core(TM) i7-7820HQ CPU @ 2.90GHz (8)
    Memory: 15.5G  free 618.4M  avail 13.5G
	
Sample non-Linux output:

    QNX 7.1.0
    Intel(R) Core(TM) i5-7400 CPU @ 3.00GHz (4)
    Memory: 7.9G  free 7.6G

### sys-cpuinfo

The output currently only works for x86 everywhere and arm Linux and QNX.

The problem with arm is that the `cpuid` equivalent is a privileged
instruction, you are not allowed to know what CPU you are running. 

Sample x86_64 output: model_name, vendor, family, model, stepping

    ("Intel(R) Core(TM) i7-3630QM CPU @ 2.40GHz" "Intel" 6 58 9)

Sample Linux arm output: model_name, vendor id, architecture, variant, part

    ("ARMv7 Processor rev 0 (v7l)" "ARM" 7 0 3081)

Sample QNX arm output: model_name, vendor, 0, 0, 0:

    ("Cortex-A72 1500MHz FPU" "AARCH64" 0 0 0)


