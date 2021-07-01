# PULP
Overview of PULP instruction extensions emitted by the compiler. These are just
raw listings, for the exact operations of these instructions consult the PULP
manuals.

## All Extension Subsets
Generate code for given RISC-V ISA (e.g. RV64IM). ISA strings must be
lower-case.

The following extensions groups are available: `xpulpv0`, `xpulpv1`,
`xpulpv2`, `xpulpv3`, `xgap8`, `corev`.

Alternatively, more fine grained subsets
can be selected with: `xpulphwloop`, `xpulppostmod`, `xpulpindregreg`,
`xpulpmacsi`, `xpulpmacrnhi`, `xpulpmulrnhi`, `xpulppartmac`, `xpulpminmax`,
`xpulpabs`, `xpulpbitop`, `xpulpbitopsmall`, `xpulpslet`, `xpulpvect`,
`xpulpvectshufflepack`, `xpulpvectgap8`, `xpulpvectall`, `xpulpclip`,
`xpulpaddsubrn`.

The following extensions are only meant for `pulpv0` and
`pulpv1` for compatibility reasons: `xpulppostmodv0`, `xpulpminmaxv0`,
`xpulpabsv0`, `xpulpmacalt`.

The assembler currently knows more extensions that then compiler. These flags
are additionally understood by the assembler:

* `xpulpvectgap9`
* `xpulpnn`
* `xpulpbitrev`
* `xpulpfinxgap9`
* `xpulpfhalfgap9`
* `xfhalf`
* `xfhalfwithf`
* `xfhalfwithd`
* `xfalthalf`
* `xfalthalfwithf`
* `xfalthalfwithd`
* `xfalthalfwithhalf`
* `xfquarter`
* `xfquarterwithf`
* `xfquarterwithd`
* `xfquarterwithhalf`
* `xfquarterwithalthalf`
* `xfvecsingle`
* `xfvecsinglenotthirtytwod`
* `xfvecsinglewithf`
* `xfvecsinglewithd`
* `xfvechalf`
* `xfvechalfnotthirtytwod`
* `xfvechalfwithf`
* `xfvechalfwithd`
* `xfvechalfwithsingle`
* `xfvecalthalf`
* `xfvecalthalfnotthirtytwod`
* `xfvecalthalfwithf`
* `xfvecalthalfwithd`
* `xfvecalthalfwithsingle`
* `xfvecalthalfwithhalf`
* `xfvecquarter`
* `xfvecquarternotthirtytwod`
* `xfvecquarterwithf`
* `xfvecquarterwithd`
* `xfvecquarterwithsingle`
* `xfvecquarterwithhalf`
* `xfvecquarterwithalthalf`
* `xfauxvecsingle`
* `xfauxhalf`
* `xfauxvechalf`
* `xfauxalthalf`
* `xfauxvecalthalf`
* `xfauxquarter`
* `xfauxvecquarter`


### Groups
There are some predefined groupings of subsets often used. The following are
known to the compiler.

The arch strings are `xpulpv0`, `xpulpv1`, `xpulpv3`, `xgap8`, `xpulpnn`, `xgap9` and `xcorev`.

  | extensions/arch         | pulpv0 | pulpv1 | pulpv2 | pulpv3 | gap8 | pulpnn | gap9 | corev |
  |-------------------------+--------+--------+--------+--------+------+--------+------+-------|
  | PULP\_POSTMOD           | x (c)  | x (C)  | x      | x      | x    | x      | x    | x     |
  | PULP\_INDREGREG         | x      | x      | x      | x      | x    | x      | x    | x     |
  | PULP\_ABS               | x (c)  | x (c)  | x      | x      | x    | x      | x    | x     |
  | PULP\_SLET              | x      | x      | x      | x      | x    | x      | x    | x     |
  | PULP\_MINMAX            | x (c)  | x (c)  | x      | x      | x    | x      | x    | x     |
  | PULP\_BITOP\_SMALL      | x      | x      |        |        |      |        |      |       |
  | PULP\_BITOP             |        |        | x      | x      | x    | x      | x    | x     |
  | PULP\_CLIP              |        |        | x      | x      | x    | x      | x    | x     |
  | PULP\_HWLOOP            | x (d)  | x      | x      | x      | x    | x      | x    | x     |
  | PULP\_MAC\_ALT          | x      | x      |        |        |      |        |      |       |
  | PULP\_MAC\_SI           |        |        | x      | x      | x    | x      | x    | x     |
  | PULP\_MACRN\_HI         |        |        | x      | x      | x    | x      | x    | x     |
  | PULP\_MULRN\_HI         |        |        | x      | x      | x    | x      | x    | x     |
  | PULP\_PARTMAC           |        |        | x      | x      | x    | x      | x    | x     |
  | PULP\_ADDSUBRN          |        |        | x      | x      | x    | x      | x    | x     |
  | PULP\_VECT              |        |        | x      | x      | x    | x      | x    | x     |
  | PULP\_VECT\_SHUFFLEPACK |        |        | x      | x      | x    | x      | x    | x     |
  | PULP\_VECT\_GAP8        |        |        |        |        | x    |        |      |       |
  | PULP\_VECT\_GAP9        |        |        |        |        |      |        | x    |       |
  | PULP\_BR                |        |        | x      | x      | x    | x      | x    | x     |
  | PULP\_ELW               | x      | x      | x      | x      | x    | x      | x    | x     |
  | PULP\_NN                |        |        |        |        |      | x      |      |       |
  | PULP\_BITREV            |        |        |        |        |      |        | x    |       |
  | PULP\_FINX\_GAP9        |        |        |        |        |      |        | x    |       |
  | PULP\_HALFFLOAT\_GAP9   |        |        |        |        |      |        | x    |       |

  c = compatibility mode
  d = disabled in compiler due to bugs

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-left" />

<col  class="org-left" />

<col  class="org-left" />

<col  class="org-left" />

<col  class="org-left" />

<col  class="org-left" />

<col  class="org-left" />

<col  class="org-left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="org-left">extensions/arch</th>
<th scope="col" class="org-left">pulpv0</th>
<th scope="col" class="org-left">pulpv1</th>
<th scope="col" class="org-left">pulpv2</th>
<th scope="col" class="org-left">pulpv3</th>
<th scope="col" class="org-left">gap8</th>
<th scope="col" class="org-left">pulpnn</th>
<th scope="col" class="org-left">gap9</th>
<th scope="col" class="org-left">corev</th>
</tr>
</thead>

<tbody>
<tr>
<td class="org-left">PULP<sub>POSTMOD</sub></td>
<td class="org-left">x (c)</td>
<td class="org-left">x (C)</td>
<td class="org-left">x</td>
<td class="org-left">x</td>
<td class="org-left">x</td>
<td class="org-left">x</td>
<td class="org-left">x</td>
<td class="org-left">x</td>
</tr>


<tr>
<td class="org-left">PULP<sub>INDREGREG</sub></td>
<td class="org-left">x</td>
<td class="org-left">x</td>
<td class="org-left">x</td>
<td class="org-left">x</td>
<td class="org-left">x</td>
<td class="org-left">x</td>
<td class="org-left">x</td>
<td class="org-left">x</td>
</tr>


<tr>
<td class="org-left">PULP<sub>ABS</sub></td>
<td class="org-left">x (c)</td>
<td class="org-left">x (c)</td>
<td class="org-left">x</td>
<td class="org-left">x</td>
<td class="org-left">x</td>
<td class="org-left">x</td>
<td class="org-left">x</td>
<td class="org-left">x</td>
</tr>


<tr>
<td class="org-left">PULP<sub>SLET</sub></td>
<td class="org-left">x</td>
<td class="org-left">x</td>
<td class="org-left">x</td>
<td class="org-left">x</td>
<td class="org-left">x</td>
<td class="org-left">x</td>
<td class="org-left">x</td>
<td class="org-left">x</td>
</tr>


<tr>
<td class="org-left">PULP<sub>MINMAX</sub></td>
<td class="org-left">x (c)</td>
<td class="org-left">x (c)</td>
<td class="org-left">x</td>
<td class="org-left">x</td>
<td class="org-left">x</td>
<td class="org-left">x</td>
<td class="org-left">x</td>
<td class="org-left">x</td>
</tr>


<tr>
<td class="org-left">PULP<sub>BITOP</sub><sub>SMALL</sub></td>
<td class="org-left">x</td>
<td class="org-left">x</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
</tr>


<tr>
<td class="org-left">PULP<sub>BITOP</sub></td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">x</td>
<td class="org-left">x</td>
<td class="org-left">x</td>
<td class="org-left">x</td>
<td class="org-left">x</td>
<td class="org-left">x</td>
</tr>


<tr>
<td class="org-left">PULP<sub>CLIP</sub></td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">x</td>
<td class="org-left">x</td>
<td class="org-left">x</td>
<td class="org-left">x</td>
<td class="org-left">x</td>
<td class="org-left">x</td>
</tr>


<tr>
<td class="org-left">PULP<sub>HWLOOP</sub></td>
<td class="org-left">x (d)</td>
<td class="org-left">x</td>
<td class="org-left">x</td>
<td class="org-left">x</td>
<td class="org-left">x</td>
<td class="org-left">x</td>
<td class="org-left">x</td>
<td class="org-left">x</td>
</tr>


<tr>
<td class="org-left">PULP<sub>MAC</sub><sub>ALT</sub></td>
<td class="org-left">x</td>
<td class="org-left">x</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
</tr>


<tr>
<td class="org-left">PULP<sub>MAC</sub><sub>SI</sub></td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">x</td>
<td class="org-left">x</td>
<td class="org-left">x</td>
<td class="org-left">x</td>
<td class="org-left">x</td>
<td class="org-left">x</td>
</tr>


<tr>
<td class="org-left">PULP<sub>MACRN</sub><sub>HI</sub></td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">x</td>
<td class="org-left">x</td>
<td class="org-left">x</td>
<td class="org-left">x</td>
<td class="org-left">x</td>
<td class="org-left">x</td>
</tr>


<tr>
<td class="org-left">PULP<sub>MULRN</sub><sub>HI</sub></td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">x</td>
<td class="org-left">x</td>
<td class="org-left">x</td>
<td class="org-left">x</td>
<td class="org-left">x</td>
<td class="org-left">x</td>
</tr>


<tr>
<td class="org-left">PULP<sub>PARTMAC</sub></td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">x</td>
<td class="org-left">x</td>
<td class="org-left">x</td>
<td class="org-left">x</td>
<td class="org-left">x</td>
<td class="org-left">x</td>
</tr>


<tr>
<td class="org-left">PULP<sub>ADDSUBRN</sub></td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">x</td>
<td class="org-left">x</td>
<td class="org-left">x</td>
<td class="org-left">x</td>
<td class="org-left">x</td>
<td class="org-left">x</td>
</tr>


<tr>
<td class="org-left">PULP<sub>VECT</sub></td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">x</td>
<td class="org-left">x</td>
<td class="org-left">x</td>
<td class="org-left">x</td>
<td class="org-left">x</td>
<td class="org-left">x</td>
</tr>


<tr>
<td class="org-left">PULP<sub>VECT</sub><sub>SHUFFLEPACK</sub></td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">x</td>
<td class="org-left">x</td>
<td class="org-left">x</td>
<td class="org-left">x</td>
<td class="org-left">x</td>
<td class="org-left">x</td>
</tr>


<tr>
<td class="org-left">PULP<sub>VECT</sub><sub>GAP8</sub></td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">x</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
</tr>


<tr>
<td class="org-left">PULP<sub>VECT</sub><sub>GAP9</sub></td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">x</td>
<td class="org-left">&#xa0;</td>
</tr>


<tr>
<td class="org-left">PULP<sub>BR</sub></td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">x</td>
<td class="org-left">x</td>
<td class="org-left">x</td>
<td class="org-left">x</td>
<td class="org-left">x</td>
<td class="org-left">x</td>
</tr>


<tr>
<td class="org-left">PULP<sub>ELW</sub></td>
<td class="org-left">x</td>
<td class="org-left">x</td>
<td class="org-left">x</td>
<td class="org-left">x</td>
<td class="org-left">x</td>
<td class="org-left">x</td>
<td class="org-left">x</td>
<td class="org-left">x</td>
</tr>


<tr>
<td class="org-left">PULP<sub>NN</sub></td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">x</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
</tr>


<tr>
<td class="org-left">PULP<sub>BITREV</sub></td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">x</td>
<td class="org-left">&#xa0;</td>
</tr>


<tr>
<td class="org-left">PULP<sub>FINX</sub><sub>GAP9</sub></td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">x</td>
<td class="org-left">&#xa0;</td>
</tr>


<tr>
<td class="org-left">PULP<sub>HALFFLOAT</sub><sub>GAP9</sub></td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">x</td>
<td class="org-left">&#xa0;</td>
</tr>
</tbody>
</table>

  c = compatibility mode
  d = disabled in compiler due to bugs


### PULP\_HWLOOP `xpulphwloop`
hardware loops

* lp.starti
* lp.endi
* lp.count
* lp.counti
* lp.setup
* lp.setupi

### PULP\_POSTMOD `xpulppostmod`
post-increment and reg-reg load and store

* p.lb
* p.lbu
* p.lh
* p.lhu
* p.lw
* p.sb
* p.sh
* p.sw

### PULP\_INDREGREG `xpulpindregreg`
register indirect store and load

### PULP\_ELW `xpulpelw`
* p.elw

### PULP\_ABS `xpulpabs`
* p.abs (collides with p.avg, only pulp < v2)

### PULP\_SLET `xpulpslet`
* p.slet
* p.sletu


### PULP\_MAC\_SI `xpulpmacsi`
mac 32x32 into 32
* p.mac
* p.msu


### PULP\_MULRN\_HI `xpulpmulrnhi`
partial mul (16x16 into 32)

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

### PULP\_MACRN\_HI `xpulpmacrnhi`
partial mac (16x16 into 32) with rounding and norm

* p.macsN
* p.macuN
* p.macsRN
* p.macuRN
* p.machhsN
* p.machhuN
* p.machhsRN
* p.machhuRN


### PULP\_PARTMAC `xpulppartmac`
partial mac 16x16 into 32

* p.macs (? not in cv32e40p)
* p.macu (? not in cv32e40p)
* p.machhs (? not in cv32e40p)
* p.machhu (? not in cv32e40p)


### PULP\_MAC\_ALT `xpulpmacalt`
Only used for pulpv0 and pulpv1 legacy

* p.macs
* p.macu
* p.machlsu
* p.machlu
* p.machhs
* p.machhu
* p.machls

### PULP\_MINMAX `xpulpminmax`
min/max functions

* p.min
* p.max
* p.minu
* p.maxu
* p.avg (replaced with p.addN in pulp >= v2) (p.addN is an emulation)
* p.avgu (replaced with p.adduN in pulp >= v2) (p.adduN is an emulation)

### PULP\_BITOP `xpulpbitop`
bit manipulation

* p.bclr
* p.bclrr
* p.bset
* p.bsetr
* p.extract*
* p.extractu*
* p.insert*

* p.cnt
* p.clb
* p.fl1
* p.ff1
* p.ror

* p.exths
* p.exthz
* p.extbs
* p.extbz

### PULP\_BITOP\_SMALL `xpulpbitopsmall`
bit manipulation subset (used in pulpv0 and pulpv1)

* p.cnt
* p.clb
* p.fl1
* p.ff1
* p.ror

* p.exths
* p.exthz
* p.extbs
* p.extbz

### PULP\_VECT `xpulpvect`
SIMD instructions (incomplete listing)

* pv.add*
* pv.pack*
* pv.packlo*
* pv.packhi*
* pv.shuffle2
* pv.extract*
* pv.extractu*
* pv.cplxconj*
* pv.sub*
* pv.subrotmj
* pv.avg*
* pv.avgu*
* pv.abs*
* pv.cplxmul*
* pv.vitop*
* pv.dotsp*
* pv.dotup*
* pv.dotusp*
* pv.cmp*

### PULP_\VECT\_SHUFFLEPACK `xpulpvectshufflepack`
Additional SIMD instructions for shuffle and pack

* pv.pack.h
* pv.packlo.b
* pv.packhi.b
* pv.shuffle.sci.h
* pv.shuffle2.h
* pv.shuffle.h
* pv.shuffle.b
* pv.shuffleI0.sci.b
* pv.shuffleI1.sci.b
* pv.shuffleI2.sci.b
* pv.shuffleI3.sci.b
* pv.shuffle2.b

Note the cv32e40p also includes these
* pv.pack
which are in PULP_VECT

### PULP_\VECT\_GAP8 `xpulpvectgap8`
Additional SIMD instructions in gap8

* pv.pack.l.h
* pv.pack.h.h
* pv.cplxconj.h
* pv.cplxconj.h
* pv.add.h.div2
* pv.add.b.div2
* pv.add.h.div4
* pv.add.b.div4
* pv.sub.h.div2
* pv.sub.b.div2
* pv.sub.h.div4
* pv.sub.b.div4
* pv.subrotmj.h
* pv.cplxmul.s
* pv.cplxmul.sci.s
* pv.cplxmul.s.div2
* pv.cplxmul.s.div4
* pv.vitop.max (with shufflepack)
* pv.vitop.sel (with shufflepack)

### PULP\_CLIP `xpulpclip`
clip instructions

* p.clip*
* p.clipu*

### PULP\_ADDSUBRN `xpulpaddsubrn`
add/sub with norm/round

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

### PULP\_BR `xpulpbr`
Immediate branching instructions

* p.beqimm
* p.bneimm

## About Group Extensions

### PULP\_V0
Compatibility mode. Broken hardware loops.

### PULP\_V1

### PULP\_V2

### PULP\_V3
Identical to `PULP_V2` execept mult is not masked/hacked out

### PULP\_NN

### GAP8
Identical to `PULP_V3` except there are additional vector instructions

### CORE\_V
Mostly identical to `PULP_V3`

### GAP9
TODO

## TODO
* [x] split pulp extensions into subextension and add extension groupings
* [x] split `*.md` patterns
* [x] add pulp-br option
* [x] fix pulp-abs option
* [x] fix clrbsi2 pattern for the case when (reg) argument is zero
