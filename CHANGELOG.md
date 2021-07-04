# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/en/1.0.0/)
and this project adheres to [Semantic Versioning](http://semver.org/spec/v2.0.0.html).


## [Unreleased]

### Changed
- gdb, sim: Rebased binutils patches

## [2.1.2] - 2021-07-03

### Added
- gcc: Tweaks for hardware loops (minimum size and alignment)

## [2.1.1] - 2021-07-03

### Fixed
- gcc: PULP builtins fail during reload/lra pass

## [2.1.0] - 2021-07-03

### Added
- `pulp.md` about new subextensions and extension groupings
- `core-v.rst` about using cv32e40p with PULP extensions
- gcc: Enable/Disable PULP extension with `-m` flags (see `--help` and `pulp.md`)
- gcc: Updated multilib settings
- gcc,binutils: Preliminary support for cv32e40p (`xcorev`)
- binutils: smach custom float extension and extension strings
- bintuils: Imported and patched corev assembly tests

### Changed
- gcc: *Split PULP extensions into various subextensions*. Check `pulp.md`.
- gcc: Split PULP specific machine descriptions into separate files
- binutils/gas: More warnings for hardware loops when used in a wrong way
- binutils: Single location for PULP extension meta information making tedious
  copy-paste-sync superfluous

### Fixed
- gcc,binutils: Version parsing of arch string (upstream bug)
- gcc: Code generation for `__builtin_clrsb`
- binutils: Disassembly of ji immeadite of hardware loops

### Removed
- gcc: Various dead `-m` options
- binutils: Dead code from custom dynamic linking

## [2.0.1] - 2020-11-19

### Changed
- Using `a` extension with `xpulpv2` is not an error anymore
- Remove redundant code that forces abi to ilp32 when using xpulp

## [2.0.0] - 2020-11-4

### Added
- gcc: port pulp patches to 9.2.0
- binutils: port pulp patches to 9.2.0
- gdb: port pulp patches to 9.2.0
- gcc: Add support xpulpv2 except vector insn (not verified) and ind-ind-reg
  insn (can't get patterns to work)
- gcc: Use new register allocator (lra) instead of the deprecated reload.
- gcc: Properly gate pulp insns behind pulp arch flags. This makes sure when
  using standard RISC-V arch flags that we clearly don't use patched code paths.
- gcc: Reintroduce default RISC-V builtins
- gdb: Add xpulpv2 insn to gdb-simulator. Used as canonical simulator to run regression tests.
- ld: Add full support for pulp relocations
- as: Remap operand shorthands of insn to resolve collisions
- as: Adapt table and glue logic to new insn class logic
- objdump: Add support for parsing of riscv-attributes to determine arch. Useful
  when trying to disassemble binaries that use insn with overlapping encodings.
- Add gitlabci flow

### Changed
- Most tools only accept lower case `-march` and `-mabi` arguments like upstream
  gcc does.
- gcc: `xpulpv2` will fail when combined with `a` extension. Previously the `a`
  extension was just silently stripped.
- objdump: insn that were previously rendered as pulp insn (prefix `p.`) but
  actually weren't are now shown in their original form (e.g. `mul` instead of
  `p.mul`)

### Fixed
- gcc: Fix `__attribute__ ((visibility ("hidden")))` logic
- gcc: Dismantle mnosext. Broken semantics and fixes regression with wrong insn
  being emitted.
- gcc: Disable `Pulp_DP_Format_type` support
- gcc: Disable mdf (internal) flag
- gcc: Remove the automatic (broken) linking to a file called `riscv.ld` in `riscv/elf.h`
- gcc: Fix conflicts in constraint naming
- gcc: Disable high part insn code path in gcc expand pass. Looks like a gcc
  bug.

### Removed
- ld: Remove lightweight dynamic linking (buggy and interfering with default
  linker code)
- gcc: Remove PULP specific interrupt handler attributes. The code was contending
  with upstream interrupt handler support.

