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
