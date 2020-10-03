# Building

## binutils
```bash
./configure --target=riscv32-unknown-elf --prefix=/home/bluew/tmp/pulp-new-gcc --disable-werror --with-expat=yes --disable-gdb --disable-libdecnumber --disable-readline
```

## gdb (needs out of tree build)
```bash
cd gdb-build-balasr
../riscv-gdb/configure --target=riscv32-unknown-elf --prefix=/home/bluew/tmp/pulp-new-gcc --disable-werror --with-expat=yes --enable-gdb --disable-gas --disable-binutils --disable-ld --disable-gold --disable-gprof
```

## dejagnu (out of tree)
```bash
cd dejagnu-build-balasr
../riscv-dejagnu/configure --prefix=/home/bluew/tmp/pulp-new-gcc
```

## qemu (as depedency for testing)
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

## gcc
```bash
cd riscv-gcc && ./contrib/download_prerequisites
cd ..
mkdir gcc-build-balasr
cd gcc-build-balasr

.././riscv-gcc/configure --target=riscv32-unknown-elf --prefix=/home/bluew/tmp/pulp-new-gcc --disable-shared --disable-threads --enable-languages=c,c++ --with-system-zlib --enable-tls --with-newlib --with-sysroot=/home/bluew/tmp/pulp-new-gcc/riscv32-unknown-elf --with-native-system-header-dir=/include --disable-libmudflap --disable-libssp --disable-libquadmath --disable-libgomp --disable-nls --disable-tm-clone-registry --src=.././riscv-gcc --enable-multilib --with-abi=ilp32 --with-arch=rv32gc --with-tune=rocket 'CFLAGS_FOR_TARGET=-Os   -mcmodel=medlow' 'CXXFLAGS_FOR_TARGET=-Os   -mcmodel=medlow'
```

# Testing
## gdb
```bash
cd gdb-build-balasr
make check RUNTEST=$HOME/tmp/pulp-new-gcc/bin/runtest RUNTESTFLAGS="--target_board=riscv-sim"
```

## binutils
```bash
cd riscv-binutils
make check RUNTEST=$HOME/tmp/pulp-new-gcc/bin/runtest RUNTESTFLAGS="--target_board=riscv-sim"
```
