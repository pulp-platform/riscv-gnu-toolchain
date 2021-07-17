;; Event Unit Read Indirect

(define_insn "load_evt_unit"
  [(set (match_operand:SI 0 "register_operand" "=&r,r")
        (unspec_volatile:SI [(match_operand:SI 1 "register_operand" "r,r") (match_operand:SI 2 "nonmemory_operand" "r,i")] UNSPECV_READ_EVU)
   )
  ]
  "TARGET_PULP_ELW"
  "@
   p.elw \t%0,%2(%1)\t# Load from Event Unit
   p.elw \t%0,%2(%1)\t# Load from Event Unit"
  [(set_attr "type" "load,load")
   (set_attr "mode" "SI,SI")]
)

;; Read/Write special purpose registers

(define_insn "read_spr"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(unspec:SI [(match_operand:SI 1 "immediate_operand" "i")] UNSPEC_SPR_READ)
   )
  ]
  "pulp_target_flags != 0"
  "csrrs \t%0,%1,x0\t# SPR read"
  [(set_attr "type" "load")
   (set_attr "mode" "SI")]
)

(define_insn "read_spr_vol"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(unspec_volatile:SI [(match_operand:SI 1 "immediate_operand" "i")] UNSPECV_SPR_READ_VOL)
   )
  ]
 "pulp_target_flags != 0"
  "csrrs \t%0,%1,x0\t# SPR read, volatile"
  [(set_attr "type" "load")
   (set_attr "mode" "SI")]
)

(define_insn "write_spr"
  [(unspec_volatile [(match_operand:SI 0 "immediate_operand" "i,i") (match_operand:SI 1 "nonmemory_operand" "r,K")] UNSPEC_SPR_WRITE)
  ]
 "pulp_target_flags != 0"
 "@
  csrrw \tx0,%0,%1\t# SPR write
  csrrwi \tx0,%0,%1\t# SPR write uimm5"
)

(define_insn "read_then_write_spr"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
	(unspec_volatile [(match_operand:SI 1 "immediate_operand" "i,i") (match_operand:SI 2 "nonmemory_operand" "r,K")] UNSPEC_SPR_WRITE)
   )
  ]
 "pulp_target_flags != 0"
 "@
  csrrw \t%0,%1,%2\t# SPR read then write
  csrrwi \t%0,%1,%2\t# SPR read then write uimm5"
)

(define_insn "spr_bit_set"
  [(unspec_volatile [(match_operand:SI 0 "immediate_operand" "i,i") (match_operand:SI 1 "nonmemory_operand" "r,K")] UNSPEC_SPR_BIT_SET)
  ]
 "pulp_target_flags != 0"
  "@
  csrrs \tx0,%0,%1\t# SPR bit set
  csrrsi \tx0,%0,%1\t# SPR bit set uimm5"
)
 
(define_insn "read_then_spr_bit_set"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
	(unspec_volatile [(match_operand:SI 1 "immediate_operand" "i,i") (match_operand:SI 2 "nonmemory_operand" "r,K")] UNSPEC_SPR_BIT_SET)
   )
  ]
 "pulp_target_flags != 0"
  "@
  csrrs \t%0,%1,%2\t# Read then SPR bit set
  csrrsi \t%0,%1,%2\t# Read then SPR bit set uimm5"
)

(define_insn "spr_bit_clr"
  [(unspec_volatile [(match_operand:SI 0 "immediate_operand" "i,i") (match_operand:SI 1 "nonmemory_operand" "r,K")] UNSPEC_SPR_BIT_CLR)
  ]
 "pulp_target_flags != 0"
  "@
  csrrc \tx0,%0,%1\t# SPR bit clr
  csrrci \tx0,%0,%1\t# SPR bit clr uimm5"
)

(define_insn "read_then_spr_bit_clr"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
	(unspec_volatile [(match_operand:SI 1 "immediate_operand" "i,i") (match_operand:SI 2 "nonmemory_operand" "r,K")] UNSPEC_SPR_BIT_CLR)
   )
  ]
 "pulp_target_flags != 0"
  "@
  csrrc \t%0,%1,%2\t# Read then SPR bit clr
  csrrci \t%0,%1,%2\t# Read then SPR bit clr uimm5"
)


;; Open MP support

(define_expand "pulp_omp_barrier"
  [(unspec_volatile [(const_int 0)] UNSPECV_OMP_PULP_BARRIER)]
  "TARGET_PULP_ELW"
{
	rtx Reg1 = gen_reg_rtx (SImode);
	rtx Reg2 = gen_reg_rtx (SImode);
	emit_insn(gen_movsi(Reg1, gen_rtx_CONST_INT(SImode, 0x00204000)));
	emit_insn(gen_load_evt_unit(Reg2, Reg1, gen_rtx_CONST_INT(SImode, 0x21c)));
	DONE;
}
)


;;(define_insn "pulp_omp_barrier"
;;  [(unspec_volatile [(const_int 0)] UNSPECV_OMP_PULP_BARRIER)
;;   (clobber (match_scratch:SI 0 "=&r"))
;;  ]
;;  "TARGET_PULP_ELW"
;;  "* return riscv_explicit_load_store(operands[0], NULL, 0x2017216, 1);"
;;)

(define_expand "pulp_omp_critical_start"
  [(unspec_volatile [(const_int 0)] UNSPECV_OMP_PULP_CRITICAL_START)]
  "TARGET_PULP_ELW"
{
	rtx Reg1 = gen_reg_rtx (SImode);
	rtx Reg2 = gen_reg_rtx (SImode);
	emit_insn(gen_movsi(Reg1, gen_rtx_CONST_INT(SImode, 0x00204000)));
	emit_insn(gen_load_evt_unit(Reg2, Reg1, gen_rtx_CONST_INT(SImode, 0xc0)));
	DONE;
}
)

;; (define_insn "pulp_omp_critical_start"
;;   [(unspec_volatile [(const_int 0)] UNSPECV_OMP_PULP_CRITICAL_START)
;;    (clobber (match_scratch:SI 0 "=&r"))
;;   ]
;;   "TARGET_PULP_ELW"
;;   "* return riscv_explicit_load_store(operands[0], gen_rtx_REG(SImode, 0), 0x2016448, 1);"
;; )

(define_insn "writesivol"
  [(unspec_volatile [(match_operand:SI 0 "register_operand" "rJ,rJ")
		     (match_operand:SI 1 "register_operand" "r,r")
		     (match_operand:SI 2 "nonmemory_operand" "r,I")] UNSPECV_WRITESI_VOL)]
 "pulp_target_flags != 0"
  "@
   p.sw \t%z0,%2(%1)\t# Write volatile
   p.sw \t%z0,%2(%1)\t# Write volatile"
  [(set_attr "type" "store,store")
   (set_attr "mode" "SI,SI")]
)

(define_insn "writesi"
  [(unspec [(match_operand:SI 0 "register_operand" "rJ,rJ")
	    (match_operand:SI 1 "register_operand" "r,r")
	    (match_operand:SI 2 "nonmemory_operand" "r,I")] UNSPEC_WRITESI)]
 "pulp_target_flags != 0"
  "@
   p.sw \t%z0,%2(%1)\t# Write non volatile
   p.sw \t%z0,%2(%1)\t# Write non volatile"
  [(set_attr "type" "store,store")

  (set_attr "mode" "SI,SI")]
)

(define_insn "readsivol"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
        (unspec_volatile:SI [(match_operand:SI 1 "register_operand" "r,r") (match_operand:SI 2 "immediate_operand" "r,i")] UNSPECV_READSI_VOL)
   )
  ]
 "pulp_target_flags != 0"
  "@
   p.lw \t%0,%2(%1)\t# Read volatile
   p.lw \t%0,%2(%1)\t# Read volatile"
  [(set_attr "type" "load,load")
   (set_attr "mode" "SI,SI")]
)

;;(define_insn "readsi"
;;  [(set (match_operand:SI 0 "register_operand" "=r,r")
;;	(mem:SI (plus:SI (match_operand:SI 1 "register_operand" "r,r") (match_operand:SI 2 "const_arith_operand" "r,i")))
;;   )
;;  ]
;; "TARGET_PULP_ELW"
;;  "@
;;   p.lw \t%0,%2(%1)\t# Read non volatile
;;   p.lw \t%0,%2(%1)\t# Read non volatile"
;;  [(set_attr "type" "load,load")
;;   (set_attr "mode" "SI,SI")]
;;)

(define_expand "pulp_omp_critical_end"
  [(unspec_volatile [(const_int 0)] UNSPECV_OMP_PULP_CRITICAL_END)]
  "TARGET_PULP_ELW"
{
	rtx Reg1 = gen_reg_rtx (SImode);
	rtx Reg2 = gen_reg_rtx (SImode);
	emit_insn(gen_movsi(Reg1, gen_rtx_CONST_INT(SImode, 0x00204000)));
	emit_insn(gen_writesivol(Reg2, Reg1, gen_rtx_CONST_INT(SImode, 0xc0)));
	DONE;
}
)

;;(define_insn "pulp_omp_critical_end"
;;  [(unspec_volatile [(const_int 0)] UNSPECV_OMP_PULP_CRITICAL_END)
;;   (clobber (match_scratch:SI 0 "=&r"))
;;  ]
;;  "TARGET_PULP_ELW"
;;  "* return riscv_explicit_load_store(operands[0], gen_rtx_REG(SImode, 0), 0x2016448, 0);"
;;)

(define_insn "OffsetedRead"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
        (unspec_volatile:SI [(match_operand:SI 1 "register_operand" "r,r") (match_operand:SI 2 "nonmemory_operand" "r,I")] UNSPECV_OFFSETED_READ)
   )
  ]
  "TARGET_PULP_POSTMOD || TARGET_PULP_INDREGREG"
  "@
   p.lw \t%0,%2(%1)\t# Volatile Load word offseted
   p.lw \t%0,%2(%1)\t# Volatile Load word offseted"
)

(define_insn "OffsetedReadHalf"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
        (unspec_volatile:SI [(match_operand:SI 1 "register_operand" "r,r") (match_operand:SI 2 "nonmemory_operand" "r,I")] UNSPECV_OFFSETED_READ_HALF)
   )
  ]
  "TARGET_PULP_POSTMOD || TARGET_PULP_INDREGREG"
  "@
   p.lh \t%0,%2(%1)\t# Volatile Load half word offseted
   p.lh \t%0,%2(%1)\t# Volatile Load half word offseted"
)

(define_insn "OffsetedReadByte"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
        (unspec_volatile:SI [(match_operand:SI 1 "register_operand" "r,r") (match_operand:SI 2 "nonmemory_operand" "r,I")] UNSPECV_OFFSETED_READ_BYTE)
   )
  ]
  "TARGET_PULP_POSTMOD || TARGET_PULP_INDREGREG"
  "@
   p.lb \t%0,%2(%1)\t# Volatile Load byte offseted
   p.lb \t%0,%2(%1)\t# Volatile Load byte offseted"
)

(define_insn "OffsetedWrite"
  [(unspec_volatile [(match_operand:SI 0 "reg_or_0_operand" "rJ,rJ")
		     (match_operand:SI 1 "register_operand" "r,r")
		     (match_operand:SI 2 "nonmemory_operand" "r,I")] UNSPECV_OFFSETED_WRITE)]
  "TARGET_PULP_POSTMOD || TARGET_PULP_INDREGREG"
  "@
   p.sw \t%z0,%2(%1)\t# Offseted Write volatile
   p.sw \t%z0,%2(%1)\t# Offseted Write volatile"
  [(set_attr "type" "store,store")
   (set_attr "mode" "SI,SI")]
)

(define_insn "OffsetedWriteHalf"
  [(unspec_volatile [(match_operand:SI 0 "reg_or_0_operand" "rJ,rJ")
		     (match_operand:SI 1 "register_operand" "r,r")
		     (match_operand:SI 2 "nonmemory_operand" "r,I")] UNSPECV_OFFSETED_WRITE_HALF)]
  "TARGET_PULP_POSTMOD || TARGET_PULP_INDREGREG"
  "@
   p.sh \t%z0,%2(%1)\t# Offseted Write Half volatile
   p.sh \t%z0,%2(%1)\t# Offseted Write Half volatile"
  [(set_attr "type" "store,store")
   (set_attr "mode" "HI,HI")]
)

(define_insn "OffsetedWriteByte"
  [(unspec_volatile [(match_operand:SI 0 "reg_or_0_operand" "rJ,rJ")
		     (match_operand:SI 1 "register_operand" "r,r")
		     (match_operand:SI 2 "nonmemory_operand" "r,I")] UNSPECV_OFFSETED_WRITE_BYTE)]
  "TARGET_PULP_POSTMOD || TARGET_PULP_INDREGREG"
  "@
   p.sb \t%z0,%2(%1)\t# Offseted Write Byte volatile
   p.sb \t%z0,%2(%1)\t# Offseted Write Byte volatile"
  [(set_attr "type" "store,store")
   (set_attr "mode" "QI,QI")]
)

(define_insn "OffsetedReadOMP"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (unspec_volatile:SI [(match_operand:SI 1 "register_operand" "r") (match_operand:SI 2 "immediate_operand" "i")] UNSPECV_OFFSETED_READ_OMP)
   )
  ]
  "TARGET_PULP_ELW"
  "p.lw \t%0,%2(%1)\t# Volatile Load offseted (OMP)"
)

(define_insn "OffsetedReadNonVol"
  [(set (match_operand:SI 0 "register_operand" "=r")
	;; (mem:SI (plus:SI (match_operand:SI 1 "register_operand" "r") (match_operand:SI 2 "immediate_operand" "i")))
	(unspec:SI [(match_operand:SI 1 "register_operand" "r") (match_operand:SI 2 "immediate_operand" "i")] UNSPEC_READSI_NONVOL)
   )
  ]
  "pulp_target_flags != 0"
  "p.lw \t%0,%2(%1)\t# Non volatile Load offseted"
)

