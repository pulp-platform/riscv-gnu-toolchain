# PULP Extensions (2021-08-11)
Overview of PULP instruction extensions. These are just raw listings for the
exact operations of these instructions consult the PULP manuals.

# Table of Contents

* [PULP Extensions (2021-08-11)](#pulp-extensions-2021-08-11)
* [Table of Contents](#table-of-contents)
* [Changelog](#changelog)
* [All Extension Subsets](#all-extension-subsets)
   * [Extension Groups](#extension-groups)
   * [xpulphwloop](#xpulphwloop)
   * [xpulppostmod](#xpulppostmod)
   * [xpulpindregreg](#xpulpindregreg)
   * [xpulpelw](#xpulpelw)
   * [xpulpabs](#xpulpabs)
   * [xpulpslet](#xpulpslet)
   * [xpulpmacsi](#xpulpmacsi)
   * [xpulpmulrnhi](#xpulpmulrnhi)
   * [xpulpmacrnhi](#xpulpmacrnhi)
   * [xpulppartmac](#xpulppartmac)
   * [xpulpmacalt](#xpulpmacalt)
   * [xpulpminmax](#xpulpminmax)
   * [xpulpbitop](#xpulpbitop)
   * [xpulpbitopsmall](#xpulpbitopsmall)
   * [xpulpvect](#xpulpvect)
   * [xpulpvectshufflepack](#xpulpvectshufflepack)
   * [xpulpvectcomplex](#xpulpvectcomplex)
   * [xpulpclip](#xpulpclip)
   * [xpulpaddsubrn](#xpulpaddsubrn)
   * [xpulpbr](#xpulpbr)
   * [xpulpbitrev](#xpulpbitrev)
   * [xpulpvectgap8](#xpulpvectgap8)
   * [xpulpvectgap9](#xpulpvectgap9)
   * [xfhalf](#xfhalf)
   * [xfalthalf](#xfalthalf)
   * [xfquarter](#xfquarter)
   * [xfvecsingle](#xfvecsingle)
   * [xfvechalf](#xfvechalf)
   * [xfvecalthalf](#xfvecalthalf)
   * [xfvecquarter](#xfvecquarter)
   * [xfauxvecsingle](#xfauxvecsingle)
   * [xfauxhalf](#xfauxhalf)
   * [xfauxvechalf](#xfauxvechalf)
   * [xfauxalthalf](#xfauxalthalf)
   * [xfauxvecalthalf](#xfauxvecalthalf)
   * [xfauxquarter](#xfauxquarter)
   * [xfauxvecquarter](#xfauxvecquarter)
   * [xpulpfinxgap9](#xpulpfinxgap9)
   * [xpulpfhalfgap9](#xpulpfhalfgap9)
* [About Group Extensions](#about-group-extensions)
   * [PULP V0](#pulp-v0)
   * [PULP V1](#pulp-v1)
   * [PULP V2](#pulp-v2)
   * [PULP V3](#pulp-v3)
   * [PULP NN](#pulp-nn)
   * [GAP8](#gap8)
   * [CORE-V](#core-v)
   * [GAP9](#gap9)

# Changelog
- `2021-08-11`: Initial release

# All Extension Subsets
Generate code for given RISC-V ISA (e.g. RV64IM). ISA strings must be
lower-case.

The following extensions groups are available: [`xpulpv0`](#pulp-v0),
[`xpulpv1`](#pulp-v1), [`xpulpv2`](#pulp-v2), [`xpulpv3`](#pulp-v3),
[`xgap8`](#gap8), [`xgap9`](#gap9) (not implemented), [`corev`](#core-v). Extension
groups select a set of extensions (i.e. are a shorthand). The following
[table](#extension-groups) for an overview of the currently available extension
groups. The compiler treats extension groups as a separate extension. This means
that arch string in RISC-V ELF attributes contains the extension group string
and not the expansion to its extension subsets.

The following extensions are only meant for `xpulpv0` and
`xpulpv1` for compatibility reasons:

* `xpulppostmodv0`
* `xpulpminmaxv0`
* `xpulpabsv0`
* `xpulpmacalt`

The following PULP extensions are available:

| Extension name                                  | Description                        | in `gcc` | in `gas` |
|-------------------------------------------------|------------------------------------|----------|----------|
| `zfinx` (replaces `f`)                          | `f` mapped to x regs  (rv32/rv64)  | [x]      | [x]      |
| `zdinx` (replaces `d`)                          | `d` mapped to x regs (rv32/rv64)   | [x]      | [x]      |
|-------------------------------------------------|------------------------------------|----------|----------|
| [`xfhalf`](#xfhalf)                             | ieee half float                    | [x]      | [x]      |
| [`xfalthalf`](#xfalthalf)                       | brain float                        | [x]      | [x]      |
| [`xfquarter`](#xfquarter)                       | custom quarter float               | [ ]      | [x]      |
| [`xfvecsingle`](#xfvecsingle)                   | SIMD single float                  | [ ]      | [x]      |
| [`xfvechalf`](#xfvechalf)                       | SIMD half float                    | [ ]      | [x]      |
| [`xfvecalthalf`](#xfvecalthalf)                 | SIMD brain float                   | [ ]      | [x]      |
| [`xfvecquarter`](#xfvecquarter)                 | SIMD quarter float                 | [ ]      | [x]      |
| [`xfhalfinx`](#xfhalf)                          | `xfhalf` mapped to x regs          | [x]      | [x]      |
| [`xfalthalfinx`](#xfalthalf)                    | `xfalthalf` mapped to x regs       | [x]      | [x]      |
| [`xfquarterinx`](#xfquarter)                    | `xfquarter` mapped to x regs       | [ ]      | [ ]      |
| [`xfvecsingleinx`](#xfvecsingle)                | `xfvecsingle` mapped to xregs      | [ ]      | [ ]      |
| [`xfvechalfinx`](#xfvechalf)                    | `xfvechalf` mapped to xregs        | [ ]      | [ ]      |
| [`xfvecalthalfinx`](#xfvecalthalf)              | `xfvecalthalf` mapped to x regs    | [ ]      | [ ]      |
| [`xfvecquarterinx`](#xfvecquarter)              | `xfvecquarter` mapped to x regs    | [ ]      | [ ]      |
|-------------------------------------------------|------------------------------------|----------|----------|
| [`xfauxhalf`](#xfauxhalf)                       | half float auxiliary               | [ ]      | [x]      |
| [`xfauxalthalf`](#xfauxalthalf)                 | brain float auxiliary              | [ ]      | [x]      |
| [`xfauxquarter`](#xfauxquarter)                 | quarter float auxiliary            | [ ]      | [x]      |
| [`xfauxvecsingle`](#xfauxvecsingle)             | SIMD single float auxiliary        | [ ]      | [x]      |
| [`xfauxvechalf`](#xfauxvechalf)                 | SIMD half float auxiliary          | [ ]      | [x]      |
| [`xfauxvecalthalf`](#xfauxvecalthalf)           | SIMD brain float auxiliary         | [ ]      | [x]      |
| [`xfauxvecquarter`](#xfauxvecquarter)           | SIMD quarter float auxiliary       | [ ]      | [x]      |
| [`xfauxhalfinx`](#xfauxhalf)                    | `xfauxhalf` mapped to x regs       | [ ]      | [ ]      |
| [`xfauxalthalfinx`](#xfauxalthalf)              | `xfauxalthalf` mapped to x regs    | [ ]      | [ ]      |
| [`xfauxquarterinx`](#xfauxquarter)              | `xfauxquarter` mapped to x regs    | [ ]      | [ ]      |
| [`xfauxvecsingleinx`](#xfauxvecsingle)          | `xfauxvecsingle` mapped to x regs  | [ ]      | [ ]      |
| [`xfauxvechalfinx`](#xfauxvechalf)              | `xfauxvechalf` mapped to x regs    | [ ]      | [ ]      |
| [`xfauxvecalthalfinx`](#xfauxvecalthalf)        | `xfauxvecalthalf` mapped to x regs | [ ]      | [ ]      |
| [`xfauxvecquarterinx`](#xfauxvecquarter)        | `xfauxvecquarter` mapped to x regs | [ ]      | [ ]      |
|-------------------------------------------------|------------------------------------|----------|----------|
| [`xpulphwloop`](#xpulphwloop)                   | hardware loops                     | [x]      | [x]      |
| [`xpulppostmod`](#xpulppostmod)                 | postmod inc/dec load/store         | [x]      | [x]      |
| [`xpulpindregreg`](#xpulpindregreg)             | indirect load/store                | [x]      | [x]      |
| [`xpulpmacsi`](#xpulpmacsi)                     | 32-bit mac                         | [x]      | [x]      |
| [`xpulpmacrnhi`](#xpulpmacrnhi)                 | 16-bit mac                         | [x]      | [x]      |
| [`xpulpmulrnhi`](#xpulpmulrnhi)                 | 16-bit mul with normal and round   | [x]      | [x]      |
| [`xpulppartmac`](#xpulppartmac)                 | partial mac                        | [x]      | [x]      |
| [`xpulpmacalt`](#xpulpmacalt)                   | mac (legacy)                       | [x]      | [x]      |
| [`xpulpminmax`](#xpulpminmax)                   | minmax                             | [x]      | [x]      |
| [`xpulpabs`](#xpulpabs)                         | abs                                | [x]      | [x]      |
| [`xpulpbitop`](#xpulpbitop)                     | bitmanipulation                    | [x]      | [x]      |
| [`xpulpbitopsmall`](#xpulpbitopsmall)           | subset of `xpulpbitop` (legacy)    | [x]      | [x]      |
| [`xpulpslet`](#xpulpslet)                       | set less than                      | [x]      | [x]      |
| [`xpulpaddsubrn`](#xpulpaddsubrn)               | add/sub with normal and round      | [x]      | [x]      |
| [`xpulpvect`](#xpulpvect)                       | SIMD integer                       | [x]      | [x]      |
| [`xpulpvectshufflepack`](#xpulpvectshufflepack) | SIMD shuffle and pack              | [x]      | [x]      |
| [`xpulpvectcomplex`](#xpulpvectcomplex)         | SIMD integer complex instructions  | [x]      | [x]      |
| [`xpulpclip`](#xpulpclip)                       | clip                               | [x]      | [x]      |
| [`xpulpbitrev`](#xpulpbitrev)                   | bitreverse instruction             | [x]      | [x]      |
| [`xpulpbr`](#xpulpbr)                           | immediate branches                 | [x]      | [x]      |
| [`xpulpelw`](#xpulpelw)                         | eu load (for minimal cluster core) | [x]      | [x]      |
|-------------------------------------------------|------------------------------------|----------|----------|
| [`xpulpvectgap8`](#xpulpvectgap8)               | SIMD integer gap8 specific         | [x]      | [x]      |
| [`xpulpvectgap9`](#xpulpvectgap9)               | SIMD integer gap9 specific         | [ ]      | [x]      |
| `xpulpnn`                                       | neural network                     | [ ]      | [x]      |
| [`xpulpfinxgap9`](#xpulpfinxgap9)               | `zfinx` gap9                       | [ ]      | [x]      |
| [`xpulpfhalfgap9`](#xpulpfhalfgap9)             | SIMD half float in x regs (gap9)   | [ ]      | [x]      |


Internally in `gcc` and `gas` the floating-point extension are split into the
proper floating-point instructions and their conversion instructions. These are
**not user visible** extensions at this moment.

For `xfhalf`

* `xfhalfwithf`
* `xfhalfwithd`

for `xfalthalf`

* `xfalthalfwithf`
* `xfalthalfwithd`
* `xfalthalfwithhalf`

For `xfquarter`

* `xfquarterwithf`
* `xfquarterwithd`
* `xfquarterwithhalf`
* `xfquarterwithalthalf`

For `xfvecsingle`

* `xfvecsinglenotthirtytwod`
* `xfvecsinglewithf`
* `xfvecsinglewithd`

For `xfvechalf`

* `xfvechalfnotthirtytwod`
* `xfvechalfwithf`
* `xfvechalfwithd`
* `xfvechalfwithsingle`

For `xfvecalthalf`

* `xfvecalthalfnotthirtytwod`
* `xfvecalthalfwithf`
* `xfvecalthalfwithd`
* `xfvecalthalfwithsingle`
* `xfvecalthalfwithhalf`

For `xfvecquarter`

* `xfvecquarternotthirtytwod`
* `xfvecquarterwithf`
* `xfvecquarterwithd`
* `xfvecquarterwithsingle`
* `xfvecquarterwithhalf`
* `xfvecquarterwithalthalf`


## Extension Groups
There are some predefined groupings of subsets often used. The following are
known to the compiler.

The arch strings are `xpulpv0`, `xpulpv1`, `xpulpv3`, `xgap8`, `xpulpnn`, `xgap9` and `xcorev`.

  | extensions/arch string                          | pulpv0 | pulpv1 | pulpv2 | pulpv3 | gap8 | pulpnn | gap9 | corev |
  |-------------------------------------------------|--------|--------|--------|--------|------|--------|------|-------|
  | [`xpulppostmod`](#xpulppostmod)                 | x (c)  | x (C)  | x      | x      | x    | x      | x    | x     |
  | [`xpulpindregreg`](#xpulpindregreg)             | x      | x      | x      | x      | x    | x      | x    | x     |
  | [`xpulpabs`](#xpulpabs)                         | x (c)  | x (c)  | x      | x      | x    | x      | x    | x     |
  | [`xpulpslet`](#xpulpslet)                       | x      | x      | x      | x      | x    | x      | x    | x     |
  | [`xpulpminmax`](#xpulpminmax)                   | x (c)  | x (c)  | x      | x      | x    | x      | x    | x     |
  | [`xpulpbitopsmall`](#xpulpbitopsmall)           | x      | x      |        |        |      |        |      |       |
  | [`xpulpbitop`](#xpulpbitop)                     |        |        | x      | x      | x    | x      | x    | x     |
  | [`xpulpclip`](#xpulpclip)                       |        |        | x      | x      | x    | x      | x    | x     |
  | [`xpulphwloop`](#xpulphwloop)                   | x (d)  | x      | x      | x      | x    | x      | x    | x     |
  | [`xpulpmacalt`](#xpulpmacalt)                   | x      | x      |        |        |      |        |      |       |
  | [`xpulpmacsi`](#xpulpmacsi)                     |        |        | x      | x      | x    | x      | x    | x     |
  | [`xpulpmacrnhi`](#xpulpmacrnhi)                 |        |        | x      | x      | x    | x      | x    | x     |
  | [`xpulpmulrnhi`](#xpulpmulrnhi)                 |        |        | x      | x      | x    | x      | x    | x     |
  | [`xpulppartmac`](#xpulppartmac)                 |        |        | x      | x      | x    | x      | x    | x     |
  | [`xpulpaddsubrn`](#xpulpaddsubrn)               |        |        | x      | x      | x    | x      | x    | x     |
  | [`xpulpvect`](#xpulpvect)                       |        |        | x      | x      | x    | x      | x    | x     |
  | [`xpulpvectshufflepack`](#xpulpvectshufflepack) |        |        | x      | x      | x    | x      | x    | x     |
  | [`xpulpvectcomplex`](#xpulpvectcomplex)         |        |        |        | x      |      | x      |      | x     |
  | [`xpulpvectgap8`](#xpulpvectgap8)               |        |        |        |        | x    |        |      |       |
  | [`xpulpvectgap9`](#xpulpvectgap9)               |        |        |        |        |      |        | x    |       |
  | [`xpulpbr`](#xpulpbr)                           |        |        | x      | x      | x    | x      | x    | x     |
  | [`xpulpelw`](#xpulpelw)                         | x      | x      | x      | x      | x    | x      | x    | x     |
  | `xpulpnn`                                       |        |        |        |        |      | x      |      |       |
  | [`xpulpbitrev`](#xpulpbitrev)                   |        |        |        |        |      |        | x    |       |
  | `xpulpfinxgap9`                                 |        |        |        |        |      |        | x    |       |
  | `xpulphalfgap9`                                 |        |        |        |        |      |        | x    |       |

  c = compatibility mode
  d = disabled in compiler due to bugs


## xpulphwloop
hardware loops (PULP\_HWLOOP in gcc)

* lp.starti
* lp.endi
* lp.count
* lp.counti
* lp.setup
* lp.setupi

## xpulppostmod
post-increment and reg-reg load and store (PULP\_POSTMOD in gcc)

* p.lb
* p.lbu
* p.lh
* p.lhu
* p.lw
* p.sb
* p.sh
* p.sw

##  xpulpindregreg
register indirect store and load (PULP\_INDREGREG in gcc)

## xpulpelw
(PULP\_ELW in gcc)

* p.elw

## xpulpabs
(PULP\_ABS in gcc)

* p.abs (collides with p.avg, only pulp < v2)

## xpulpslet
(PULP\_SLET  in gcc)

* p.slet
* p.sletu


## xpulpmacsi
mac 32x32 into 32 (PULP\_MAC\_SI in gcc)

* p.mac
* p.msu


## xpulpmulrnhi
partial mul (16x16 into 32) (PULP\_MULRN\_HI in gcc)

* p.mulhhs
* p.mulhhu
* p.muls
* p.mulu
* p.mulsN
* p.mulsRN
* p.muluN
* p.muluRN
* p.mulhhsN
* p.mulhhuN
* p.mulhhsRN
* p.mulhhuRN

## xpulpmacrnhi
partial mac (16x16 into 32) with rounding and norm (PULP\_MACRN\_HI in gcc)

* p.macsN
* p.macuN
* p.macsRN
* p.macuRN
* p.machhsN
* p.machhuN
* p.machhsRN
* p.machhuRN


## xpulppartmac
partial mac 16x16 into 32 (PULP\_PARTMAC in gcc)

* p.macs (not in cv32e40p)
* p.macu (not in cv32e40p)
* p.machhs (not in cv32e40p)
* p.machhu (not in cv32e40p)


## xpulpmacalt
Only used for pulpv0 and pulpv1 legacy (PULP\_MAC\_ALT in gcc)

* p.macs
* p.macu
* p.machlsu
* p.machlu
* p.machhs
* p.machhu
* p.machls

## xpulpminmax
min/max functions (PULP\_MINMAX in gcc)

* p.min
* p.max
* p.minu
* p.maxu
* p.avg (replaced with p.addN in pulp >= v2) (p.addN is an emulation)
* p.avgu (replaced with p.adduN in pulp >= v2) (p.adduN is an emulation)

## xpulpbitop
bit manipulation (PULP\_BITOP in gcc)

* p.extract
* p.extracti
* p.extractr
* p.extractu
* p.extractui
* p.extractur
* p.insert
* p.inserti
* p.insertr
* p.bset
* p.bseti
* p.bsetr
* p.bclr
* p.bclri
* p.bclrr
* p.cnt
* p.clb
* p.fl1
* p.ff1
* p.ror
* p.exths
* p.exthz
* p.extbs
* p.extbz

## xpulpbitopsmall
bit manipulation subset (used in pulpv0 and pulpv1) (PULP\_BITOP\_SMALL in gcc)

* p.cnt
* p.clb
* p.fl1
* p.ff1
* p.ror
* p.exths
* p.exthz
* p.extbs
* p.extbz

## xpulpvect
Integer SIMD instructions (PULP\_VECT in gcc)

* pv.add.h
* pv.add.sc.h
* pv.add.sci.h
* pv.add.b
* pv.add.sc.b
* pv.add.sci.b
* pv.sub.h
* pv.sub.sc.h
* pv.sub.sci.h
* pv.sub.b
* pv.sub.sc.b
* pv.sub.sci.b
* pv.avg.h
* pv.avg.sc.h
* pv.avg.sci.h
* pv.avg.b
* pv.avg.sc.b
* pv.avg.sci.b
* pv.avgu.h
* pv.avgu.sc.h
* pv.avgu.sci.h
* pv.avgu.b
* pv.avgu.sc.b
* pv.avgu.sci.b
* pv.min.h
* pv.min.sc.h
* pv.min.sci.h
* pv.min.b
* pv.min.sc.b
* pv.min.sci.b
* pv.minu.h
* pv.minu.sc.h
* pv.minu.sci.h
* pv.minu.b
* pv.minu.sc.b
* pv.minu.sci.b
* pv.max.h
* pv.max.sc.h
* pv.max.sci.h
* pv.max.b
* pv.max.sc.b
* pv.max.sci.b
* pv.maxu.h
* pv.maxu.sc.h
* pv.maxu.sci.h
* pv.maxu.b
* pv.maxu.sc.b
* pv.maxu.sci.b
* pv.srl.h
* pv.srl.sc.h
* pv.srl.sci.h
* pv.srl.b
* pv.srl.sc.b
* pv.srl.sci.b
* pv.sra.h
* pv.sra.sc.h
* pv.sra.sci.h
* pv.sra.b
* pv.sra.sc.b
* pv.sra.sci.b
* pv.sll.h
* pv.sll.sc.h
* pv.sll.sci.h
* pv.sll.b
* pv.sll.sc.b
* pv.sll.sci.b
* pv.or.h
* pv.or.sc.h
* pv.or.sci.h
* pv.or.b
* pv.or.sc.b
* pv.or.sci.b
* pv.xor.h
* pv.xor.sc.h
* pv.xor.sci.h
* pv.xor.b
* pv.xor.sc.b
* pv.xor.sci.b
* pv.and.h
* pv.and.sc.h
* pv.and.sci.h
* pv.and.b
* pv.and.sc.b
* pv.and.sci.b
* pv.abs.h
* pv.abs.b
* pv.extract.h
* pv.extract.b
* pv.extractu.h
* pv.extractu.b
* pv.insert.h
* pv.insert.b
* pv.dotsp.h
* pv.dotsp.sc.h
* pv.dotsp.sci.h
* pv.dotsp.b
* pv.dotsp.sc.b
* pv.dotsp.sci.b
* pv.dotup.h
* pv.dotup.sc.h
* pv.dotup.sci.h
* pv.dotup.b
* pv.dotup.sc.b
* pv.dotup.sci.b
* pv.dotusp.h
* pv.dotusp.sc.h
* pv.dotusp.sci.h
* pv.dotusp.b
* pv.dotusp.sc.b
* pv.dotusp.sci.b
* pv.sdotsp.h
* pv.sdotsp.sc.h
* pv.sdotsp.sci.h
* pv.sdotsp.b
* pv.sdotsp.sc.b
* pv.sdotsp.sci.b
* pv.sdotup.h
* pv.sdotup.sc.h
* pv.sdotup.sci.h
* pv.sdotup.b
* pv.sdotup.sc.b
* pv.sdotup.sci.b
* pv.sdotusp.h
* pv.sdotusp.sc.h
* pv.sdotusp.sci.h
* pv.sdotusp.b
* pv.sdotusp.sc.b
* pv.sdotusp.sci.b
* pv.cmpeq.h
* pv.cmpeq.sc.h
* pv.cmpeq.sci.h
* pv.cmpeq.b
* pv.cmpeq.sc.b
* pv.cmpeq.sci.b
* pv.cmpne.h
* pv.cmpne.sc.h
* pv.cmpne.sci.h
* pv.cmpne.b
* pv.cmpne.sc.b
* pv.cmpne.sci.b
* pv.cmpgt.h
* pv.cmpgt.sc.h
* pv.cmpgt.sci.h
* pv.cmpgt.b
* pv.cmpgt.sc.b
* pv.cmpgt.sci.b
* pv.cmpge.h
* pv.cmpge.sc.h
* pv.cmpge.sci.h
* pv.cmpge.b
* pv.cmpge.sc.b
* pv.cmpge.sci.b
* pv.cmplt.h
* pv.cmplt.sc.h
* pv.cmplt.sci.h
* pv.cmplt.b
* pv.cmplt.sc.b
* pv.cmplt.sci.b
* pv.cmple.h
* pv.cmple.sc.h
* pv.cmple.sci.h
* pv.cmple.b
* pv.cmple.sc.b
* pv.cmple.sci.b
* pv.cmpgtu.h
* pv.cmpgtu.sc.h
* pv.cmpgtu.sci.h
* pv.cmpgtu.b
* pv.cmpgtu.sc.b
* pv.cmpgtu.sci.b
* pv.cmpgeu.h
* pv.cmpgeu.sc.h
* pv.cmpgeu.sci.h
* pv.cmpgeu.b
* pv.cmpgeu.sc.b
* pv.cmpgeu.sci.b
* pv.cmpltu.h
* pv.cmpltu.sc.h
* pv.cmpltu.sci.h
* pv.cmpltu.b
* pv.cmpltu.sc.b
* pv.cmpltu.sci.b
* pv.cmpleu.h
* pv.cmpleu.sc.h
* pv.cmpleu.sci.h
* pv.cmpleu.b
* pv.cmpleu.sc.b
* pv.cmpleu.sci.b
* pv.add.h.div2
* pv.add.h.div4
* pv.add.h.div8
* pv.sub.h.div2
* pv.sub.h.div4
* pv.sub.h.div8

## xpulpvectshufflepack
Additional SIMD instructions for shuffle and pack (PULP_\VECT\_SHUFFLEPACK in gcc)

* pv.shuffle.h
* pv.shuffle.sci.h
* pv.shuffle.b
* pv.shufflei0.sci.b
* pv.shufflei1.sci.b
* pv.shufflei2.sci.b
* pv.shufflei3.sci.b
* pv.shuffle2.h
* pv.shuffle2.b
* pv.pack
* pv.pack.h
* pv.packhi.b
* pv.packlo.b

Note the cv32e40p also includes this instruction
* pv.pack


##  xpulpvectcomplex
PULP SIMD complex instructions (PULP_\VECT\_COMPLEX in gcc)

* pv.cplxmul.h.r
* pv.cplxmul.h.r.div
* pv.cplxmul.h.r.div
* pv.cplxmul.h.r.div
* pv.cplxmul.h.i
* pv.cplxmul.h.i.div
* pv.cplxmul.h.i.div
* pv.cplxmul.h.i.div
* pv.subrotmj.h
* pv.subrotmj.h.div2
* pv.subrotmj.h.div4
* pv.subrotmj.h.div8
* pv.cplxconj.h


## xpulpclip
clip instructions (PULP\_CLIP in gcc)

* p.clip
* p.clipi
* p.clipr
* p.clipu
* p.clipui
* p.clipur

## xpulpaddsubrn
add/sub with norm/round (PULP\_ADDSUBRN in gcc)

* p.addn
* p.addni
* p.addnr
* p.addun
* p.adduni
* p.addunr
* p.addrn
* p.addrni
* p.addrnr
* p.addurn
* p.addurni
* p.addurnr
* p.subn
* p.subni
* p.subnr
* p.subun
* p.subuni
* p.subunr
* p.subrn
* p.subrni
* p.subrnr
* p.suburn
* p.suburni
* p.suburnr

## xpulpbr
Immediate branching instructions (PULP\_BR in gcc)

* p.beqimm
* p.bneimm

## xpulpbitrev
Immediate branching instructions (PULP\_BITREV in gcc)

* p.bitrev

## xpulpvectgap8
Additional SIMD instructions in gap8 (PULP_\VECT\_GAP8 in gcc)

* pv.pack.h.h
* pv.pack.l.h
* pv.cplxmul.s
* pv.cplxmul.sc.s
* pv.cplxmul.sci.s
* pv.cplxmul.s.div2
* pv.cplxmul.s.div4
* pv.subrotmj.h
* pv.subrotmj.h.div2
* pv.subrotmj.h.div4
* pv.add.b.div2
* pv.add.b.div4
* pv.sub.b.div2
* pv.sub.b.div4
* pv.vitop.max (with shuffle pack)
* pv.vitop.sel (with shuffle pack)
* pv.cplxconj.h

## xpulpvectgap9
Additional SIMD instructions in gap9 (PULP_\VECT\_GAP9 in gcc)

* pv.cplxmul.h.r
* pv.cplxmul.h.r.div2
* pv.cplxmul.h.r.div4
* pv.cplxmul.h.r.div8
* pv.cplxmul.h.i
* pv.cplxmul.h.i.div2
* pv.cplxmul.h.i.div4
* pv.cplxmul.h.i.div8
* pv.subrotmj.h
* pv.subrotmj.h.div2
* pv.subrotmj.h.div4
* pv.subrotmj.h.div8
* pv.cplxconj.h
* pv.add.h.div2
* pv.add.h.div4
* pv.add.h.div8
* pv.sub.h.div2
* pv.sub.h.div4
* pv.sub.h.div8
* pv.pack.h.h


## xfhalf
IEEE half-precision floats. Same as `xfhalfinx`

* flh
* flh
* fsh
* fsh
* fmadd.h
* fmadd.h
* fmsub.h
* fmsub.h
* fnmsub.h
* fnmsub.h
* fnmadd.h
* fnmadd.h
* fadd.h
* fadd.h
* fsub.h
* fsub.h
* fmul.h
* fmul.h
* fdiv.h
* fdiv.h
* fsqrt.h
* fsqrt.h
* fsgnj.h
* fsgnjn.h
* fsgnjx.h
* fmin.h
* fmax.h
* feq.h
* flt.h
* fle.h
* fcvt.w.h
* fcvt.w.h
* fcvt.wu.h
* fcvt.wu.h
* fcvt.h.w
* fcvt.h.w
* fcvt.h.wu
* fcvt.h.wu
* fmv.x.h
* fclass.h
* fmv.h.x
* fgt.h
* fge.h
* fmv.h
* fabs.h
* fneg.h

RV64 only

* fcvt.l.h
* fcvt.l.h
* fcvt.lu.
* fcvt.lu.
* fcvt.h.l
* fcvt.h.l
* fcvt.h.l
* fcvt.h.l

Conversions

* fcvt.s.h"
* fcvt.h.s"
* fcvt.h.s"
* fcvt.d.h"
* fcvt.h.d"
* fcvt.h.d"


## xfalthalf
Brain float. Same as `xfalthalfinx`

* flah
* flah
* fsah
* fsah
* fmadd.ah
* fmsub.ah
* fnmsub.ah
* fnmadd.ah
* fadd.ah
* fsub.ah
* fmul.ah
* fdiv.ah
* fsqrt.ah
* fsgnj.ah
* fsgnjn.ah
* fsgnjx.ah
* fmin.ah
* fmax.ah
* feq.ah
* flt.ah
* fle.ah
* fcvt.w.ah
* fcvt.wu.ah
* fcvt.ah.w
* fcvt.ah.wu
* fmv.x.ah
* fclass.ah
* fmv.ah.x
* fgt.ah
* fge.ah
* fmv.ah
* fabs.ah
* fneg.ah

RV64 only

* fcvt.l.ah
* fcvt.lu.ah
* fcvt.ah.l
* fcvt.ah.lu

Conversions

* fcvt.s.ah
* fcvt.ah.s
* fcvt.d.ah
* fcvt.ah.d
* fcvt.h.ah
* fcvt.h.ah
* fcvt.ah.h

## xfquarter
Quarter float. Same as `xfquarterinx`

* flb
* flb
* fsb
* fsb
* fmadd.b
* fmadd.b
* fmsub.b
* fmsub.b
* fnmsub.b
* fnmsub.b
* fnmadd.b
* fnmadd.b
* fadd.b
* fadd.b
* fsub.b
* fsub.b
* fmul.b
* fmul.b
* fdiv.b
* fdiv.b
* fsqrt.b
* fsqrt.b
* fsgnj.b
* fsgnjn.b
* fsgnjx.b
* fmin.b
* fmax.b
* feq.b
* flt.b
* fle.b
* fcvt.w.b
* fcvt.w.b
* fcvt.wu.b
* fcvt.wu.b
* fcvt.b.w
* fcvt.b.w
* fcvt.b.wu
* fcvt.b.wu
* fmv.x.b
* fclass.b
* fmv.b.x
* fgt.b
* fge.b
* fmv.b
* fabs.b
* fneg.b

RV64 only

* fcvt.l.b
* fcvt.l.b
* fcvt.lu.b
* fcvt.lu.b
* fcvt.b.l
* fcvt.b.l
* fcvt.b.lu
* fcvt.b.lu

Conversions

* fcvt.s.b
* fcvt.b.s
* fcvt.b.s
* fcvt.d.b
* fcvt.b.d
* fcvt.b.d
* fcvt.h.b
* fcvt.b.h
* fcvt.b.h
* fcvt.ah.b
* fcvt.b.ah
* fcvt.b.ah

## xfvecsingle
Single float SIMD. Same as `xfquarterinx`. Requires `FLEN>=64`

* vfadd.s
* vfadd.r.s
* vfsub.s
* vfsub.r.s
* vfmul.s
* vfmul.r.s
* vfdiv.s
* vfdiv.r.s
* vfmin.s
* vfmin.r.s
* vfmax.s
* vfmax.r.s
* vfsqrt.s
* vfmac.s
* vfmac.r.s
* vfmre.s
* vfmre.r.s
* vfclass.s
* vfsgnj.s
* vfsgnj.r.s
* vfsgnjn.s
* vfsgnjn.r.s
* vfsgnjx.s
* vfsgnjx.r.s
* vfeq.s
* vfeq.r.s
* vfne.s
* vfne.r.s
* vflt.s
* vflt.r.s
* vfge.s
* vfge.r.s
* vfle.s
* vfle.r.s
* vfgt.s
* vfgt.r.s
* vfabs.s

Unless RV32D

* vfneg.s
* vfmv.x.s
* vfmv.s.x
* vfcvt.x.s
* vfcvt.xu.s
* vfcvt.s.x
* vfcvt.s.xu

Conversions

* vfcpka.s.s
* vfcpkb.s.s
* vfcpkc.s.s
* vfcpkd.s.s
* vfcpka.s.d
* vfcpkb.s.d
* vfcpkc.s.d
* vfcpkd.s.d

## xfvechalf
IEEE half float SIMD. Same as `xfvechalfinx`. Requires `FLEN>=32`

* vfadd.h
* vfadd.r.h
* vfsub.h
* vfsub.r.h
* vfmul.h
* vfmul.r.h
* vfdiv.h
* vfdiv.r.h
* vfmin.h
* vfmin.r.h
* vfmax.h
* vfmax.r.h
* vfsqrt.h
* vfmac.h
* vfmac.r.h
* vfmre.h
* vfmre.r.h
* vfclass.h
* vfsgnj.h
* vfsgnj.r.h
* vfsgnjn.h
* vfsgnjn.r.h
* vfsgnjx.h
* vfsgnjx.r.h
* vfeq.h
* vfeq.r.h
* vfne.h
* vfne.r.h
* vflt.h
* vflt.r.h
* vfge.h
* vfge.r.h
* vfle.h
* vfle.r.h
* vfgt.h
* vfgt.r.h
* vfabs.h
* vfneg.h

Unless RV32D

* vfmv.x.h
* vfmv.h.x
* vfcvt.x.h
* vfcvt.xu.h
* vfcvt.h.x
* vfcvt.h.xu

Conversions

* vfcpka.h.s
* vfcpkb.h.s
* vfcpkc.h.s
* vfcpkd.h.s
* vfcpka.h.d
* vfcpkb.h.d
* vfcpkc.h.d
* vfcpkd.h.d
* vfcvt.s.h
* vfcvtu.s.h
* vfcvt.h.s
* vfcvtu.h.s

## xfvecalthalf
Brain float SIMD. Same as `xfvecalthalf`. Requires `FLEN>=32`

* vfadd.ah
* vfadd.r.ah
* vfsub.ah
* vfsub.r.ah
* vfmul.ah
* vfmul.r.ah
* vfdiv.ah
* vfdiv.r.ah
* vfmin.ah
* vfmin.r.ah
* vfmax.ah
* vfmax.r.ah
* vfsqrt.ah
* vfmac.ah
* vfmac.r.ah
* vfmre.ah
* vfmre.r.ah
* vfclass.ah
* vfsgnj.ah
* vfsgnj.r.ah
* vfsgnjn.ah
* vfsgnjn.r.ah
* vfsgnjx.ah
* vfsgnjx.r.ah
* vfeq.ah
* vfeq.r.ah
* vfne.ah
* vfne.r.ah
* vflt.ah
* vflt.r.ah
* vfge.ah
* vfge.r.ah
* vfle.ah
* vfle.r.ah
* vfgt.ah
* vfgt.r.ah
* vfabs.ah
* vfneg.ah

Unless RV32D

* vfmv.x.ah
* vfmv.ah.x
* vfcvt.x.ah
* vfcvt.xu.ah
* vfcvt.ah.x
* vfcvt.ah.xu

Conversions

* vfcpka.ah.s
* vfcpkb.ah.s
* vfcpkc.ah.s
* vfcpkd.ah.s
* vfcpka.ah.d
* vfcpkb.ah.d
* vfcpkc.ah.d
* vfcpkd.ah.d
* vfcvt.s.ah
* vfcvtu.s.ah
* vfcvt.ah.s
* vfcvtu.ah.s
* vfcvt.h.ah
* vfcvtu.h.ah
* vfcvt.ah.h
* vfcvtu.ah.h

## xfvecquarter
Quarter float SIMD. Same as `xfvecquarterinx`. Requires `FLEN>=16`

* vfadd.b
* vfadd.r.b
* vfsub.b
* vfsub.r.b
* vfmul.b
* vfmul.r.b
* vfdiv.b
* vfdiv.r.b
* vfmin.b
* vfmin.r.b
* vfmax.b
* vfmax.r.b
* vfsqrt.b
* vfmac.b
* vfmac.r.b
* vfmre.b
* vfmre.r.b
* vfclass.b
* vfsgnj.b
* vfsgnj.r.b
* vfsgnjn.b
* vfsgnjn.r.b
* vfsgnjx.b
* vfsgnjx.r.b
* vfeq.b
* vfeq.r.b
* vfne.b
* vfne.r.b
* vflt.b
* vflt.r.b
* vfge.b
* vfge.r.b
* vfle.b
* vfle.r.b
* vfgt.b
* vfgt.r.b
* vfabs.b
* vfneg.b

Unless RV32D

* vfmv.x.b
* vfmv.b.x
* vfcvt.x.b
* vfcvt.xu.b
* vfcvt.b.x
* vfcvt.b.xu

Conversions

* vfcpka.b.s
* vfcpkb.b.s
* vfcpkc.b.s
* vfcpkd.b.s
* vfcpka.b.d
* vfcpkb.b.d
* vfcpkc.b.d
* vfcpkd.b.d
* vfcvt.s.b
* vfcvtu.s.b
* vfcvt.b.s
* vfcvtu.b.s
* vfcvt.h.b
* vfcvtu.h.b
* vfcvt.b.h
* vfcvtu.b.h
* vfcvt.ah.b
* vfcvtu.ah.b
* vfcvt.b.ah
* vfcvtu.b.ah

## xfauxvecsingle
Single float auxiliary SIMD.

* vfdotp.s
* vfdotp.r.s
* vfavg.s
* vfavg.r.s

## xfauxhalf
IEEE half float auxiliary

* fmulex.s.h
* fmulex.s.h
* fmacex.s.h
* fmacex.s.h

## xfauxvechalf
IEEE half float auxiliary SIMD

* vfdotp.h
* vfdotp.r.h
* vfdotpex.s.h
* vfdotpex.s.r.h
* vfavg.h
* vfavg.r.h

## xfauxalthalf
IEEE brain float auxiliary

* fmulex.s.ah
* fmacex.s.ah

## xfauxvecalthalf
IEEE brain float auxiliary SIMD

* vfdotp.ah
* vfdotp.r.ah
* vfdotpex.s.ah
* vfdotpex.s.r.ah
* vfavg.ah
* vfavg.r.ah

## xfauxquarter
IEEE quarter float auxiliary

* fmulex.s.b
* fmulex.s.b
* fmacex.s.b
* fmacex.s.b

## xfauxvecquarter
IEEE quarter float auxiliary SIMD

* vfdotp.b
* vfdotp.r.b
* vfdotpex.s.b
* vfdotpex.s.r.b
* vfavg.b
* vfavg.r.b

## xpulpfinxgap9
`f` instructions mapped to integer register. `gap9` only. Needs to be treated
separately in the assembler due to hardcoded stuff.

## xpulpfhalfgap9
Brain float extension. `gap9` only.

* fmadd.h
* fmadd.h
* fmsub.h
* fmsub.h
* fnmsub.h
* fnmsub.h
* fnmadd.h
* fnmadd.h
* fadd.h
* fadd.h
* fsub.h
* fsub.h
* fmul.h
* fmul.h
* fdiv.h
* fdiv.h
* fsqrt.h
* fsqrt.h
* fsgnj.h
* fsgnjn.h
* fsgnjx.h
* fmin.h
* fmax.h
* feq.h
* flt.h
* fle.h
* fcvt.w.h
* fcvt.w.h
* fcvt.wu.h
* fcvt.wu.h
* fcvt.h.w
* fcvt.h.w
* fcvt.h.wu
* fcvt.h.wu
* fcvt.s.h
* fcvt.h.s
* fcvt.h.s
* fclass.h
* fgt.h
* fge.h
* fabs.h
* fneg.h
* fcvt.h.ah
* fcvt.h.ah
* fcvt.ah.h
* fmulex.s.ah
* fmacex.s.ah
* vfadd.ah
* vfadd.r.ah
* vfsub.ah
* vfsub.r.ah
* vfmul.ah
* vfmul.r.ah
* vfmin.ah
* vfmin.r.ah
* vfmax.ah
* vfmax.r.ah
* vfmac.ah
* vfmac.r.ah
* vfmre.ah
* vfmre.r.ah
* vfclass.ah
* vfsgnj.ah
* vfsgnj.r.ah
* vfsgnjn.ah
* vfsgnjn.r.ah
* vfsgnjx.ah
* vfsgnjx.r.ah
* vfeq.ah
* vfeq.r.ah
* vfne.ah
* vfne.r.ah
* vflt.ah
* vflt.r.ah
* vfge.ah
* vfge.r.ah
* vfle.ah
* vfle.r.ah
* vfgt.ah
* vfgt.r.ah
* vfcpka.ah.s
* vfcvt.x.ah
* vfcvt.xu.ah
* vfcvt.ah.x
* vfcvt.ah.xu
* vfabs.ah
* vfneg.ah
* vfadd.h
* vfadd.r.h
* vfsub.h
* vfsub.r.h
* vfmul.h
* vfmul.r.h
* vfmin.h
* vfmin.r.h
* vfmax.h
* vfmax.r.h
* vfmac.h
* vfmac.r.h
* vfmre.h
* vfmre.r.h
* vfclass.h
* vfsgnj.h
* vfsgnj.r.h
* vfsgnjn.h
* vfsgnjn.r.h
* vfsgnjx.h
* vfsgnjx.r.h
* vfeq.h
* vfeq.r.h
* vfne.h
* vfne.r.h
* vflt.h
* vflt.r.h
* vfge.h
* vfge.r.h
* vfle.h
* vfle.r.h
* vfgt.h
* vfgt.r.h
* vfcpka.h.s
* vfcvt.x.h
* vfcvt.xu.h
* vfcvt.h.x
* vfcvt.h.xu
* vfabs.h
* vfneg.h

# About Group Extensions

## PULP V0
Compatibility mode. Broken hardware loops.

## PULP V1
Compatibility mode.

## PULP V2

## PULP V3
Identical to `PULP_V2` execept mult is not masked/hacked out

## PULP NN
TODO

## GAP8
Identical to `PULP_V2` except there are additional integer simd instructions.

## CORE-V
Mostly identical to `PULP_V3`, except that the hardware loop body is more
constrained. See the cv32e40p manual.

## GAP9
TODO
