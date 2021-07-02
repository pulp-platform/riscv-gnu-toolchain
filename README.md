PULP RISC-V GNU Compiler Toolchain
=============================
[![Build status](https://iis-git.ee.ethz.ch/gnu/riscv-gnu-toolchain/badges/master/pipeline.svg)](https://iis-git.ee.ethz.ch/gnu/riscv-gnu-toolchain/-/commits/master)

This is the PULP RISC-V C and C++ cross-compiler a generic ELF/Newlib toolchain.

###  Getting the sources

This repository uses submodules. You need the --recursive option to fetch the submodules automatically

    $ git clone --recursive https://iis-git.ee.ethz.ch/gnu/riscv-gnu-toolchain/
    
Alternatively :

    $ git clone https://iis-git.ee.ethz.ch/gnu/riscv-gnu-toolchain/
    $ cd riscv-gnu-toolchain
    $ git submodule update --init --recursive
    

### Prerequisites

Several standard packages are needed to build the toolchain.  On Ubuntu,
executing the following command should suffice:

    $ sudo apt-get install autoconf automake autotools-dev curl python3 libmpc-dev libmpfr-dev libgmp-dev gawk build-essential bison flex texinfo gperf libtool patchutils bc zlib1g-dev libexpat-dev

On Fedora/CentOS/RHEL OS, executing the following command should suffice:

    $ sudo yum install autoconf automake python3 libmpc-devel mpfr-devel gmp-devel gawk  bison flex texinfo patchutils gcc gcc-c++ zlib-devel expat-devel

On OS X, you can use [Homebrew](http://brew.sh) to install the dependencies:

    $ brew install python3 gawk gnu-sed gmp mpfr libmpc isl zlib expat

To build the glibc (Linux) on OS X, you will need to build within a
case-sensitive file system. The simplest approach is to create and mount a new
disk image with a case sensitive format. Make sure that the mount point does not
contain spaces. This is not necessary to build newlib or gcc itself on OS X.

This process will start by downloading about 200 MiB of upstream sources, then
will patch, build, and install the toolchain.  If a local cache of the
upstream sources exists in $(DISTDIR), it will be used; the default location
is /var/cache/distfiles.  Your computer will need about 8 GiB of disk space to
complete the process.

### Installation (Newlib)

To build the Newlib cross-compiler, pick an install path.  If you choose,
say, `/opt/riscv`, then add `/opt/riscv/bin` to your `PATH` now.  Then, simply
run the following command:

    ./configure --prefix=/opt/riscv --march=rv32imfcxpulpv2 --mabi=ilp32e --enable-multilib
    make

You should now be able to use riscv32-unknown-elf-gcc and its cousins. You can
omit `--enable-multilib` if you are only interested in the specific
`-march`/`-mabi` combination.

Supported ABIs are ilp32 (32-bit soft-float), ilp32d (32-bit hard-float), ilp32f
(32-bit with single-precision in registers and double in memory, niche use
only), and ilp32e (embedded abi, fewer caller-saved registers for better
interrupt latency)

### Troubleshooting Build Problems

Builds work best if installing into an empty directory.  If you build a
hard-float toolchain and then try to build a soft-float toolchain with
the same --prefix directory, then the build scripts may get confused
and exit with a linker error complaining that hard float code can't be
linked with soft float code.  Removing the existing toolchain first, or
using a different prefix for the second build, avoids the problem.  It
is OK to build one newlib and one linux toolchain with the same prefix.
But you should avoid building two newlib or two linux toolchains with
the same prefix.

Centos (and RHEL) provide old GNU tools versions that may be too old to build
a RISC-V toolchain.  There is an alternate toolset provided that includes
current versions of the GNU tools.  This is the devtoolset provided as part
of the Software Collection service.  For more info, see the
[devtoolset-7](https://www.softwarecollections.org/en/scls/rhscl/devtoolset-7/)
URL.  There are various versions of the devtoolset that are available, so you
can also try other versions of it, but we have at least one report that
devtoolset-7 works.

### Advanced Options

There are a number of additional options that may be passed to
configure.  See './configure --help' for more details.

### Running the Test Suite

The DejaGnu test suite has been ported to RISC-V.  This can run with GDB
simulator or QEMU.
To test GCC, run the following commands:

    ./configure --prefix=$RISCV --with-arch=rv32ima
	make report-gcc-newlib # or make report-gcc-newlib-qemu
	make report-binutils-newlib
	make report-gdb-newlib

### Supported PULP Extensions

Read `pulp.md` for information about the supported PULP extensions. Read
`core-v.rst` to see how these extension can be used with the cv32e40p.


Check `riscv32-unknown-elf-gcc --target=help` for supported options in the compiler,
`riscv32-unknown-elf-gcc-as --help` for supported options in the assembler.

In the riscv-isa-manual you can check out how to write ISA strings that specify
the enabled extensions. In general, custom extension are denoted by a leading
`x` followed the extension name. Extension are separated by and underscore `_`
and you can also specify version numbers by appending a string `xpy` e.g. `2p0`
do specify version `2.0`. Here a few quick commands:

* `riscv32-unknown-elf-gcc -march=rv32imfcxpulpv3 FILES`: rv32 with the `imfc`
  standard extension and the `pulpv3` extension group.

* `riscv32-unknown-elf-gcc -march=rv32imc_xpulphwloops_xpulppostmod FILES`: rv32 with
  `imc` standard extension, pulp hardware loop and pulp load-store post modify
  instructions.

Sometimes you want to run with an extension group such as `xpulpv3` but disable
certain subsets e.g. hardware loops. Do it like this:

* `riscv32-unknown-elf-gcc -march=rv32imfc_xpulpv3 -mno-pulp-hwloop FILES`

Check `riscv32-unknown-elf-gcc --target=help` for more such switches.
