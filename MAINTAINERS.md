# Information for Maintainers

## Making a binary Release

1. Make sure you build on Centos 7. This makes it so that glibc requirements are low.
2. Make a gitlab release
3. Tag the branch with `TAG` (follow semantic versioning)
4. For the configure command, make sure you `--enable-multilib` and set
   `--with-pkgversion="$DATE PULP GCC $TAG"`. This should look like this

```bash
 ./configure --prefix=/scratch/balasr-build/pulp-gcc-2.1.2 --with-arch=rv32imfcxpulpv3 --with-abi=ilp32 --enable-multilib --with-pkgversion=2021-07-04 PULP GCC v2.1.2`
```

5. Make a tarball of the install directory using the following naming scheme

```bash
tar -czvf pulp-gcc-$TAG-$DATE.tar.gz -C $PATH_TO_INSTALL_DIR
```

## Building
General notes on building the GNU Compiler Collection.

### binutils
```bash
./configure --target=riscv32-unknown-elf --prefix=/home/bluew/tmp/pulp-new-gcc --disable-werror --with-expat=yes --disable-gdb --disable-libdecnumber --disable-readline
```

### gdb (needs out of tree build)
```bash
cd gdb-build-balasr
../riscv-gdb/configure --target=riscv32-unknown-elf --prefix=/home/bluew/tmp/pulp-new-gcc --disable-werror --with-expat=yes --enable-gdb --disable-gas --disable-binutils --disable-ld --disable-gold --disable-gprof
```

### dejagnu (out of tree)
```bash
cd dejagnu-build-balasr
../riscv-dejagnu/configure --prefix=/home/bluew/tmp/pulp-new-gcc
```

### qemu (as depedency for testing)
	Needs to be patched
	1. `--disable-werror` because deprecation warnings are upgraded to errors
	2. `--disable-docs` because newer sphinx versions are not able to build the docs
```bash
stamps/build-qemu: $(srcdir)/qemu
	rm -rf $@ $(notdir $@)
	mkdir $(notdir $@)
	cd $(notdir $@) && $</configure \
		--prefix=$(INSTALL_DIR) \
		--target-list=riscv64-linux-user,riscv32-linux-user \
		--interp-prefix=$(INSTALL_DIR)/sysroot \
		--disable-docs \
		--disable-werror \
		--python=python2
	$(MAKE) -C $(notdir $@)
	$(MAKE) -C $(notdir $@) install
	mkdir -p $(dir $@)
	date > $@
```

### Targets
good target board `riscv-sim/-march=rv32imafc/-mabi=ilp32f/-mcmodel=medlow`

### gcc
```bash
cd riscv-gcc && ./contrib/download_prerequisites
cd ..
mkdir gcc-build-balasr
cd gcc-build-balasr

.././riscv-gcc/configure --target=riscv32-unknown-elf --prefix=/home/bluew/tmp/pulp-new-gcc --disable-shared --disable-threads --enable-languages=c,c++ --with-system-zlib --enable-tls --with-newlib --with-sysroot=/home/bluew/tmp/pulp-new-gcc/riscv32-unknown-elf --with-native-system-header-dir=/include --disable-libmudflap --disable-libssp --disable-libquadmath --disable-libgomp --disable-nls --disable-tm-clone-registry --src=.././riscv-gcc --enable-multilib --with-abi=ilp32 --with-arch=rv32gc --with-tune=rocket 'CFLAGS_FOR_TARGET=-Os   -mcmodel=medlow' 'CXXFLAGS_FOR_TARGET=-Os   -mcmodel=medlow'
```

## Testing
How to test various parts of the GNU Compiler Collection.

### gdb
```bash
cd gdb-build-balasr
make check RUNTEST=$HOME/tmp/pulp-new-gcc/bin/runtest RUNTESTFLAGS="--target_board=riscv-sim"
```

### binutils
```bash
cd riscv-binutils
make check RUNTEST=$HOME/tmp/pulp-new-gcc/bin/runtest RUNTESTFLAGS="--target_board=riscv-sim"
```

### gcc

```bash
# example on how to run tls.exp
cd build-gcc-newlib-stage2/
cd gcc
export PATH="/scratch/balasr/riscv-gnu-toolchain/../toolchain/bin:$PATH"

# if want QEMU
export PATH="/scratch/balasr/riscv-gnu-toolchain/scripts/wrapper/qemu:$PATH"
export RISC_V_SYSROOT="/scratch/balasr/riscv-gnu-toolchain/../toolchain/sysroot"

# make sure that you have a site.exp file that defines 'target_triplet', 'src_dir' etc.
# if it doesn't exist, you can just run
make report-gcc
# which will generate these files for you

# all the tests are relative to /scratch/balasr/riscv-gnu-toolchain/build-gcc-newlib-stage2/gcc
# make sure you have site.exp at the same location
runtest --tool gcc --target_board='riscv-sim/-march=rv32imafc/-mabi=ilp32f/-mcmodel=medlow' gcc.dg/torture/tls/tls.exp
runtest --tool gcc --target_board='riscv-sim/-march=rv32imafc/-mabi=ilp32f/-mcmodel=medlow' gcc.c-torture/compile/compile.exp
runtest --tool gcc --target_board='riscv-sim/-march=rv32imafc/-mabi=ilp32f/-mcmodel=medlow' gcc.dg/torture/dg-torture.exp
make -C build-gcc-newlib-stage2 check-gcc "RUNTESTFLAGS=--target_board='riscv-sim/-march=rv32imfcxpulpv2/-mabi=ilp32f/-mcmodel=medlow'"
```

### Debug Snippets

Stringify macros
```c
#define xstr(a) str(a)
#define str(a) #a
#pragma message ("defined=" xstr(ASM_OUTPUT_EXTERNAL(a,b,c)))
#error abort for debugging
```

Dump and debug gcc tree
```c
printf("fndecl\n");
debug_tree(fndecl);
printf("fnptrtype\n");
debug_tree(fnptrtype);
```
