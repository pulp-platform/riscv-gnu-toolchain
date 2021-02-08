;;
;;  ....................
;;
;;	BIT INSERT/EXTRACT/EXTRACTU/SET/CLEAR
;;
;;  ....................

;; PULP only

(define_insn "bclrsi3"
  [(set	(match_operand:SI 0 "register_operand" "=r")
	(and:SI	(match_operand:SI 1 "register_operand" "r")
		(match_operand:SI 2 "immediate_operand" "i")
	)
   )
  ]
"((Pulp_Cpu>=PULP_V2) && !TARGET_MASK_NOBITOP && riscv_valid_bit_field_imm_operand(operands[2], NULL, 0, NULL, NULL))"
{
	int Offset, Size;
	rtx xoperands[5];

	(void) riscv_valid_bit_field_imm_operand(operands[2], NULL, 0, &Size, &Offset);
	
	xoperands[0] = operands[0];
	xoperands[1] = operands[1];
	xoperands[2] = operands[2];
	xoperands[3] = gen_rtx_CONST_INT (SImode, Size-1);
	xoperands[4] = gen_rtx_CONST_INT (SImode, Offset);
	output_asm_insn("p.bclr \t%0,%1,%3,%4 # Bit clear", xoperands);
	return "";
}
[(set_attr "type" "logical")
 (set_attr "mode" "SI")]
)

;; Size: R2[9..5], Offset: R2[4..0]
;; R0 = R1 & ~((1<<((R2>>5)&0x1F)-1)<<(R2&0x1F)
(define_insn "bclrsi3_reg"
  [(set	(match_operand:SI 0 "register_operand" "=r")
	(and:SI	(match_operand:SI 1 "register_operand" "r")
		(not:SI (ashift:SI
				(minus:SI
					(ashift:SI (const_int 1)
						   (plus:SI
					   	   	(and:SI (lshiftrt:SI (match_operand:SI 2 "register_operand" "r") (const_int 5)) (const_int 31))
							(const_int 1)
						   )
					)
					(const_int 1)
				)
				(and:SI (match_dup 2) (const_int 31))
			)
		)
	)
   )
  ]
  "((Pulp_Cpu>=PULP_V2) && !TARGET_MASK_NOBITOP)"
  "p.bclrr\\t%0,%1,%2 # Bit clear reg"
[(set_attr "type" "logical")
 (set_attr "mode" "SI")]
)

(define_insn "bsetsi3"
  [(set	(match_operand:SI 0 "register_operand" "=r")
	(ior:SI	(match_operand:SI 1 "register_operand" "r")
		(match_operand:SI 2 "immediate_operand" "i")
	)
   )
  ]
"((Pulp_Cpu>=PULP_V2) && !TARGET_MASK_NOBITOP && riscv_valid_bit_field_imm_operand(operands[2], NULL, 1, NULL, NULL))"
{
	int Offset, Size;
	rtx xoperands[5];

	(void) riscv_valid_bit_field_imm_operand(operands[2], NULL, 1, &Size, &Offset);
	xoperands[0] = operands[0];
	xoperands[1] = operands[1];
	xoperands[2] = operands[2];
	xoperands[3] = gen_rtx_CONST_INT (SImode, Size-1);
	xoperands[4] = gen_rtx_CONST_INT (SImode, Offset);
	output_asm_insn("p.bset \t%0,%1,%3,%4 # Bit set", xoperands);
	return "";
}
[(set_attr "type" "logical")
 (set_attr "mode" "SI")]
)

;; Size: R2[9..5], Offset: R2[4..0]
;; R0 = R1 | ((1<<((R2>>5)&0x1F)-1)<<(R2&0x1F)
(define_insn "bsetsi3_reg"
  [(set	(match_operand:SI 0 "register_operand" "=r")
	(ior:SI	(match_operand:SI 1 "register_operand" "r")
		(ashift:SI
			(minus:SI
				(ashift:SI (const_int 1)
					   (plus:SI
				   	   	(and:SI (lshiftrt:SI (match_operand:SI 2 "register_operand" "r") (const_int 5)) (const_int 31))
						(const_int 1)
					   )
				)
				(const_int 1)
			)
			(and:SI (match_dup 2) (const_int 31))
		)
	)
   )
  ]
  "((Pulp_Cpu>=PULP_V2) && !TARGET_MASK_NOBITOP)"
  "p.bsetr\\t%0,%1,%2 # Bit set reg"
[(set_attr "type" "logical")
 (set_attr "mode" "SI")]
)

(define_insn "extvsi"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (sign_extract:SI (match_operand:SI 1 "register_operand" "r")
                         (match_operand:SI 2 "immediate_operand" "i")
                         (match_operand:SI 3 "immediate_operand" "i")))]
  "((Pulp_Cpu>=PULP_V2) && !TARGET_MASK_NOBITOP)"
{
	operands[2] = GEN_INT(INTVAL(operands[2])-1);
 	return "p.extract \t%0,%1,%2,%3 # Bit extract signed";
}
  [(set_attr "type" "logical")
   (set_attr "length" "1")]
)


;; Size: R2[9..5], Offset: R2[4..0]
(define_insn "bextracts_reg_si3"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (sign_extract:SI (match_operand:SI 1 "register_operand" "r")
			 (plus:SI (and:SI (lshiftrt:SI (match_operand:SI 2 "register_operand" "r") (const_int 5)) (const_int 31))
				  (const_int 1))
			 (and:SI (match_dup 2) (const_int 31))
	)
   )
  ]
  "((Pulp_Cpu>=PULP_V2) && !TARGET_MASK_NOBITOP)"
  "p.extractr \t%0,%1,%2 # Bit extract signed, arg reg"
  [(set_attr "type" "logical")
   (set_attr "length" "1")]
)

;; Size: R3[9..5], Offset: R3[4..0]
(define_insn "bextracts_reg_alt_si3"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
        (unspec:SI [(match_operand:SI 1 "register_operand" "r,r")
		    (match_operand:SI 2 "nonmemory_operand" "r,i")] UNSPEC_BEXTS_REG)
   )
  ]
  "((Pulp_Cpu>=PULP_V2) && !TARGET_MASK_NOBITOP)"
{
  if (which_alternative == 0) {
  	return "p.extractr \t%0,%1,%2 # Bit extract signed, arg reg";
  } else {
	rtx xoperands[4];
	HOST_WIDE_INT Mask = INTVAL(operands[2]);

	xoperands[0] = operands[0]; xoperands[1] =
	xoperands[2] = gen_rtx_CONST_INT (SImode, (Mask>>5)&0x1F);
	xoperands[3] = gen_rtx_CONST_INT (SImode, (Mask)&0x1F);
	output_asm_insn("p.extract\t%0,%1,%2,%3 # Bit extract signed", xoperands);
	return "";
  }
}
[(set_attr "type" "logical,logical")
 (set_attr "mode" "SI")]
)

;; Size: R3[9..5], Offset: R3[4..0]
(define_insn "bextractu_reg_alt_si3"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
        (unspec:SI [(match_operand:SI 1 "register_operand" "r,r")
		    (match_operand:SI 2 "nonmemory_operand" "r,i")] UNSPEC_BEXTU_REG)
   )
  ]
  "((Pulp_Cpu>=PULP_V2) && !TARGET_MASK_NOBITOP)"
{
  if (which_alternative == 0) {
  	return "p.extractur \t%0,%1,%2 # Bit extract unsigned, reg arg";
  } else {
	rtx xoperands[4];
	HOST_WIDE_INT Mask = INTVAL(operands[2]);

	xoperands[0] = operands[0]; xoperands[1] =
	xoperands[2] = gen_rtx_CONST_INT (SImode, (Mask>>5)&0x1F);
	xoperands[3] = gen_rtx_CONST_INT (SImode, (Mask)&0x1F);
	output_asm_insn("p.extractu\t%0,%1,%2,%3 # Bit extract unsigned", xoperands);
	return "";
  }
}
[(set_attr "type" "logical,logical")
 (set_attr "mode" "SI")]
)

(define_insn "extzvsi"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (zero_extract:SI (match_operand:SI 1 "register_operand" "r")
                         (match_operand:SI 2 "immediate_operand" "i")
                         (match_operand:SI 3 "immediate_operand" "i")))]
  "((Pulp_Cpu>=PULP_V2) && !TARGET_MASK_NOBITOP)"
{
	operands[2] = GEN_INT(INTVAL(operands[2])-1);
  	return "p.extractu \t%0,%1,%2,%3 # Bit extract unsigned";
}
[(set_attr "type" "logical")
 (set_attr "mode" "SI")]
)

;; Size: R2[9..5], Offset: R2[4..0]
(define_insn "bextractu_reg_si3"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (zero_extract:SI (match_operand:SI 1 "register_operand" "r")
			 (plus:SI (and:SI (lshiftrt:SI (match_operand:SI 2 "register_operand" "r") (const_int 5)) (const_int 31))
				  (const_int 1))
			 (and:SI (match_dup 2) (const_int 31))
	)
   )
  ]
  "((Pulp_Cpu>=PULP_V2) && !TARGET_MASK_NOBITOP)"
  "p.extractur \t%0,%1,%2 # Bit extract unsigned, reg arg"
[(set_attr "type" "logical")
 (set_attr "mode" "SI")]
)

(define_insn "insvsi"
  [(set (zero_extract:SI (match_operand:SI 0 "register_operand" "+r")
                         (match_operand:SI 1 "immediate_operand" "i")
                         (match_operand:SI 2 "immediate_operand" "i"))
        (match_operand:SI 3 "reg_or_0_operand" "rJ")
   )
  ]
  "((Pulp_Cpu>=PULP_V2) && !TARGET_MASK_NOBITOP)"
{
	operands[1] = GEN_INT(INTVAL(operands[1])-1);
  	if (operands[3] == CONST0_RTX (GET_MODE (operands[3])))
  		return "p.insert\t%0,x0,%1,%2";
  	else return "p.insert\t%0,%3,%1,%2";
}
[(set_attr "type" "logical")
 (set_attr "mode" "SI")]
)

(define_insn "*insvsi_internal1"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (ior:SI (and:SI (match_operand:SI 1 "register_operand" "0")
                        (match_operand:SI 2 "immediate_operand" "i"))
                (and:SI (match_operand:SI 3 "reg_or_0_operand" "rJ")
                        (match_operand:SI 4 "immediate_operand" "i"))))]
  "((Pulp_Cpu>=PULP_V2) && !TARGET_MASK_NOBITOP) && riscv_bottom_bitmask_p (INTVAL (operands[4]))
   && INTVAL(operands[2]) == ~INTVAL(operands[4])"
{
  int len, pos;
  pos = riscv_bitmask (INTVAL (operands[4]), &len, SImode);
  operands[2] = GEN_INT (pos);
  operands[4] = GEN_INT (len-1);
  if (operands[3] == CONST0_RTX (GET_MODE (operands[3])))
  	return "p.insert\t%0,x0,%4,%2";
  else return "p.insert\t%0,%3,%4,%2";
}
[(set_attr "type" "logical")
 (set_attr "mode" "SI")]
)

(define_insn "*insvsi_internal2"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (ior:SI (and:SI (match_operand:SI 1 "reg_or_0_operand" "rJ")
                        (match_operand:SI 2 "immediate_operand" "i"))
                (and:SI (match_operand:SI 3 "register_operand" "0")
                        (match_operand:SI 4 "immediate_operand" "i"))))]
  "((Pulp_Cpu>=PULP_V2) && !TARGET_MASK_NOBITOP) && riscv_bottom_bitmask_p (INTVAL (operands[2]))
   && INTVAL(operands[2]) == ~INTVAL(operands[4])"
{
  int len, pos;
  pos = riscv_bitmask (INTVAL (operands[2]), &len, SImode);
  operands[2] = GEN_INT (pos);
  operands[4] = GEN_INT (len-1);
  if (operands[1] == CONST0_RTX (GET_MODE (operands[1])))
  	return "p.insert\t%0,x0,%4,%2";
  else return "p.insert\t%0,%1,%4,%2";
}
[(set_attr "type" "logical")
 (set_attr "mode" "SI")]
)

(define_insn "invsipat1"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (ior:SI (and:SI (match_operand:SI 1 "register_operand" "0")
                        (match_operand:SI 2 "immediate_operand" "i"))
                (and:SI (ashift:SI (match_operand:SI 3 "reg_or_0_operand" "rJ")
			           (match_operand:SI 5 "immediate_operand" "i"))
                        (match_operand:SI 4 "immediate_operand" "i"))))]
  "((Pulp_Cpu>=PULP_V2) && !TARGET_MASK_NOBITOP)
   && riscv_bitmask (INTVAL (operands[4]), NULL, VOIDmode) == INTVAL (operands[5])
   && INTVAL(operands[2]) == ~INTVAL(operands[4])"
{
  int len;
  riscv_bitmask (INTVAL (operands[4]), &len, SImode);
  operands[4] = GEN_INT (len-1);
  if (operands[3] == CONST0_RTX (GET_MODE (operands[3])))
  	return "p.insert\t%0,x0,%4,%5";
  else return "p.insert\t%0,%3,%4,%5";
}
[(set_attr "type" "logical")
 (set_attr "mode" "SI")]
)

;; Size: R3[9..5], Offset: R3[4..0]
(define_insn "binsert_reg_si3"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
        (unspec:SI [(match_operand:SI 1 "register_operand" "0,0")
		    (match_operand:SI 2 "reg_or_0_operand" "rJ,rJ")
		    (match_operand:SI 3 "nonmemory_operand" "r,i")] UNSPEC_BINS_REG)
   )
  ]
  "((Pulp_Cpu>=PULP_V2) && !TARGET_MASK_NOBITOP)"
{
  if (which_alternative == 0) {
  	return "p.insertr\t%0,%z2,%3";
  } else {
	rtx xoperands[5];
	HOST_WIDE_INT Mask = INTVAL(operands[3]);

	xoperands[0] = operands[0]; xoperands[1] = operands[1]; xoperands[2] = operands[2];
	xoperands[3] = gen_rtx_CONST_INT (SImode, (Mask>>5)&0x1F);
	xoperands[4] = gen_rtx_CONST_INT (SImode, (Mask)&0x1F);
	output_asm_insn("p.insert\t%0,%z2,%3,%4", xoperands);
	return "";
  }
}
[(set_attr "type" "logical,logical")
 (set_attr "mode" "SI")]
)

(define_insn "*insvsi_internal4"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (ior:SI (and:SI (ashift:SI (match_operand:SI 3 "reg_or_0_operand" "rJ")
				   (match_operand:SI 5 "immediate_operand" "i"))
                        (match_operand:SI 4 "immediate_operand" "i"))
	 	(and:SI (match_operand:SI 1 "register_operand" "0")
                        (match_operand:SI 2 "immediate_operand" "i"))))]
  "((Pulp_Cpu>=PULP_V2) && !TARGET_MASK_NOBITOP)
   && riscv_bitmask (INTVAL (operands[4]), NULL, VOIDmode) == INTVAL (operands[5])
   && INTVAL(operands[2]) == ~INTVAL(operands[4])"
{
  int len;
  riscv_bitmask (INTVAL (operands[4]), &len, SImode);
  operands[4] = GEN_INT (len-1);
  if (operands[3] == CONST0_RTX (GET_MODE (operands[3])))
  	return "p.insert\t%0,x0,%4,%5";
  else return "p.insert\t%0,%3,%4,%5";
}
[(set_attr "type" "logical")
 (set_attr "mode" "SI")]
)

(define_insn "*insvsi_internal5"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (ior:SI (and:SI (match_operand:SI 1 "register_operand" "0")
                        (match_operand:SI 2 "immediate_operand" "i"))
                (ashift:SI (match_operand:SI 3 "reg_or_0_operand" "rJ")
			   (match_operand:SI 4 "immediate_operand" "i"))))]
  "((Pulp_Cpu>=PULP_V2) && !TARGET_MASK_NOBITOP) && riscv_bitmask_ins_p (INTVAL (operands[2]), INTVAL (operands[4]), SImode)"
{
  int len;
  riscv_bitmask (~INTVAL (operands[2]), &len, SImode);
  operands[2] = GEN_INT (len-1);
  if (operands[3] == CONST0_RTX (GET_MODE (operands[3])))
  	return "p.insert\t%0,x0,%2,%4";
  else return "p.insert\t%0,%3,%2,%4";
}
[(set_attr "type" "logical")
 (set_attr "mode" "SI")]
)

(define_insn "*insvsi_internal4"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (ior:SI (ashift:SI (match_operand:SI 3 "reg_or_0_operand" "rJ")
			   (match_operand:SI 4 "immediate_operand" "i"))
		(and:SI (match_operand:SI 1 "register_operand" "0")
                        (match_operand:SI 2 "immediate_operand" "i"))))]
  "((Pulp_Cpu>=PULP_V2) && !TARGET_MASK_NOBITOP) && riscv_bitmask_ins_p (INTVAL (operands[2]), INTVAL (operands[4]), SImode)"
{
  int len;
  riscv_bitmask (~INTVAL (operands[2]), &len, SImode);
  operands[2] = GEN_INT (len-1);
  if (operands[3] == CONST0_RTX (GET_MODE (operands[3])))
  	return "p.insert\t%0,x0,%2,%4";
  else return "p.insert\t%0,%3,%2,%4";
}
[(set_attr "type" "logical")
 (set_attr "mode" "SI")]
)

