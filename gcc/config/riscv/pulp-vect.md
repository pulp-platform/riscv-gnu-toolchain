;;
;;  ....................
;;
;;	VECTOR OPERATIONS
;;
;;  ....................

;; PULP only

(define_mode_iterator VMODEINT          [V2HI V4QI])
(define_mode_iterator VMODEALL          [V2HI V4QI])
(define_mode_iterator VMODEALL4         [V4QI])
(define_mode_iterator VMODEALL2         [V2HI])

(define_mode_attr VINT       [(V2HI "V2HI") (V4QI "V4QI")])
(define_mode_attr vec_type   [(V2HI "v2hi") (V4QI "v4qi")])
(define_mode_attr vec_size   [(V2HI "h")    (V4QI "b")])

(define_mode_attr vec_scalar          [(V2HI "SI")  (V4QI "SI")])
(define_mode_attr vec_scalar_int      [(V2HI "SI")  (V4QI "SI")])
(define_mode_attr vec_scalar_elmt     [(V2HI "HI")  (V4QI "QI")])

;; Vector Init

(define_insn "vec_init<VMODEALL:mode>_internal"
 [(set (match_operand:VMODEALL 0 "register_operand" "=r,r")
       (vec_duplicate:VMODEALL (match_operand:<vec_scalar_elmt> 1 "nonmemory_operand" "r,vIsdc"))
  )
 ]
"TARGET_PULP_VECT"
"@
  pv.add.sc.<vec_size>\t%0,x0,%1 # Vector insert Scalar Reg
  pv.add.sci.<vec_size>\t%0,x0,%W1 # Vector insert Scalar Imm"

[(set_attr "type" "move,move")
 (set_attr "mode" "SI,SI")]
)

(define_expand "vec_init<VMODEALL:mode>"
  [(match_operand:VMODEALL 0 "register_operand" "")
   (match_operand 1 "" "")]
"TARGET_PULP_VECT"
{
  riscv_expand_vector_init (operands[0], operands[1]);
  DONE;
}
)

;; Vector Packing

(define_insn "vec_pack_v2hi"
  [(set	(match_operand:V2HI 0 "register_operand" "=r")
	(vec_concat:V2HI
		(match_operand:HI 1 "register_operand" "r")
		(match_operand:HI 2 "register_operand" "r")
	)
   )
  ]
  "TARGET_PULP_VECT_SHUFFLEPACK"
  "pv.pack.h \t%0,%2,%1 \t# Vector pack of 2 shorts"
[(set_attr "type" "move")
 (set_attr "mode" "SI")]
)

(define_insn "vec_pack_v4qi_lo"
  [(set (match_operand:V4QI 0 "register_operand" "=r")
	(vec_merge:V4QI
		(vec_concat:V4QI
			(vec_concat:V2QI
				(match_operand:QI 1 "register_operand" "r")
				(match_operand:QI 2 "register_operand" "r")
			)
			(const_vector:V2QI [(const_int 0) (const_int 0)])
		)
          	(match_operand:V4QI 3 "register_operand" "0")
		(const_int 3)
	)
   )
  ]
  "TARGET_PULP_VECT_SHUFFLEPACK"
  "pv.packlo.b \t%0,%2,%1 \t# Vector pack of 2 bytes, low part"
[(set_attr "type" "move")
 (set_attr "mode" "SI")]
)

(define_insn "vec_pack_v4qi_lo_first"
  [(set (match_operand:V4QI 0 "register_operand" "=r")
	(vec_merge:V4QI
		(vec_concat:V4QI
			(vec_concat:V2QI
				(match_operand:QI 1 "register_operand" "r")
				(match_operand:QI 2 "register_operand" "r")
			)
			(const_vector:V2QI [(const_int 0) (const_int 0)])
		)
	  	(const_vector:V4QI [(const_int 0) (const_int 0) (const_int 0) (const_int 0)])
		(const_int 3)
	)
   )
  ]
  "TARGET_PULP_VECT_SHUFFLEPACK"
  "pv.packlo.b \t%0,%2,%1 \t# Vector pack of 2 bytes (first), low part"
[(set_attr "type" "move")
 (set_attr "mode" "SI")]
)


(define_insn "vec_pack_v4qi_hi"
  [(set	(match_operand:V4QI 0 "register_operand" "=r")
	(vec_merge:V4QI
		(vec_concat:V4QI
			(const_vector:V2QI [(const_int 0) (const_int 0)])
			(vec_concat:V2QI
				(match_operand:QI 1 "register_operand" "r")
				(match_operand:QI 2 "register_operand" "r")
			)
		)
          	(match_operand:V4QI 3 "register_operand" "0")
		(const_int 12)
	)
   )
  ]
  "TARGET_PULP_VECT_SHUFFLEPACK"
  "pv.packhi.b \t%0,%2,%1 \t# Vector pack of 2 bytes, high part"
[(set_attr "type" "move")
 (set_attr "mode" "SI")]
)

(define_insn "vec_pack_v4qi_hi_first"
  [(set	(match_operand:V4QI 0 "register_operand" "=r")
	(vec_merge:V4QI
		(vec_concat:V4QI
			(const_vector:V2QI [(const_int 0) (const_int 0)])
			(vec_concat:V2QI
				(match_operand:QI 1 "register_operand" "r")
				(match_operand:QI 2 "register_operand" "r")
			)
		)
	  	(const_vector:V4QI [(const_int 0) (const_int 0) (const_int 0) (const_int 0)])
		(const_int 12)
	)
   )
  ]
  "TARGET_PULP_VECT_SHUFFLEPACK"
  "pv.packhi.b \t%0,%2,%1 \t# Vector pack of 2 bytes (first), high part"
[(set_attr "type" "move")
 (set_attr "mode" "SI")]
)


(define_expand "vec_pack_v4qi"
  [(match_operand:V4QI 0 "register_operand" "")
   (match_operand:QI 1 "register_operand" "")
   (match_operand:QI 2 "register_operand" "")
   (match_operand:QI 3 "register_operand" "")
   (match_operand:QI 4 "register_operand" "")
  ]
  "TARGET_PULP_VECT_SHUFFLEPACK"
{
	emit_insn (gen_vec_pack_v4qi_lo_first(operands[0], operands[1], operands[2]));
	emit_insn (gen_vec_pack_v4qi_hi      (operands[0], operands[3], operands[4], operands[0]));
  DONE;
})

;; Vector permutation

(define_insn "vec_permv2hi_internal2_1"
  [(set (match_operand:V2HI 0 "register_operand"               "=r,r")
        (unspec:V2HI [(match_operand:V2HI 1 "register_operand"  "r,r")
                      (match_operand:V2HI 2 "register_operand"  "1,1")
                      (match_operand:V2HI 3 "permute_sel_operand" "r,i")
		     ] UNSPEC_VEC_PERM2)
   )
  ]
  "TARGET_PULP_VECT_SHUFFLEPACK && riscv_valid_permute_operands (operands[1], operands[2], operands[3])"
{
	switch (which_alternative) {
		case 0:
			return "pv.shuffle.h\t%0,%1,%3";
		case 1:
			{
				int Mask=0;
				rtx xoperands[3];
				int i;

				xoperands[0] = operands[0]; xoperands[1] = operands[2];
  				for (i = 0; i < 2; ++i) Mask |= (((INTVAL (XVECEXP (operands[3], 0, i)) & 1))<<(4*i));
				Mask = Mask & 0x0FF;
				xoperands[2] = gen_rtx_CONST_INT (SImode, Mask);
				output_asm_insn("pv.shuffle.sci.h\t%0,%1,%2", xoperands);
				return "";
			}
		default:
			return "";
	}
}
[(set_attr "type" "move,move")
 (set_attr "mode" "SI,SI")]
)

(define_insn "vec_permv2hi_internal2"
  [(set (match_operand:V2HI 0 "register_operand"               "=r")
        (unspec:V2HI [(match_operand:V2HI 1 "register_operand" "0")
                      (match_operand:V2HI 2 "register_operand" "r")
                      (match_operand:V2HI 3 "register_operand" "r")
		     ] UNSPEC_VEC_PERM3)
   )
  ]
  "TARGET_PULP_VECT_SHUFFLEPACK"
  "pv.shuffle2.h\t%0,%2,%3 \t# Shuffle2, word"
[(set_attr "type" "move")
 (set_attr "mode" "SI")]
)

(define_insn "vec_permv2hi_int1"
  [(set (match_operand:V2HI 0 "register_operand"               "=r,r")
        (unspec:V2HI [(match_operand:V2HI 1 "register_operand"  "r,r")
                      (match_operand:V2HI 2 "permute_sel_operand" "r,i")
		     ] UNSPEC_VEC_PERM1)
   )
  ]
  "TARGET_PULP_VECT_SHUFFLEPACK"
{
	switch (which_alternative) {
		case 0:
			return "pv.shuffle.h\t%0,%1,%2";
		case 1: {
				unsigned int Mask=0;
				rtx xoperands[3];
				int i;

				xoperands[0] = operands[0]; xoperands[1] = operands[1];
  				for (i = 0; i < 2; ++i) Mask |= (((INTVAL (XVECEXP (operands[2], 0, i)) & 1))<<(4*i));
				Mask = Mask & 0x0FF;
				xoperands[2] = gen_rtx_CONST_INT (SImode, Mask);
				output_asm_insn("pv.shuffle.sci.h\t%0,%1,%2", xoperands);
				return "";
			}
		default:
			return "";
	}
}
[(set_attr "type" "move,move")
 (set_attr "mode" "SI,SI")]
)

/* __GAP8 Start */

(define_insn "vec_permv2hi_low"
  [(set (match_operand:V2HI 0 "register_operand"                  "=r,r")
        (unspec:V2HI [(match_operand:V2HI 1 "register_operand"    "r,r")
                      (match_operand:V2HI 2 "permute_sel_operand" "r,i")
                     ] UNSPEC_VEC_PERM4)
   )
  ]
  "(TARGET_PULP_VECT_GAP8 && TARGET_PULP_VECT_SHUFFLEPACK)"
  "pv.pack.l.h \t%0,%2,%1 \t# Pack2 low"
[(set_attr "type" "move,move")
 (set_attr "mode" "SI,SI")]
)

(define_insn "vec_permv2hi_high"
  [(set (match_operand:V2HI 0 "register_operand"                  "=r,r")
        (unspec:V2HI [(match_operand:V2HI 1 "register_operand"    "r,r")
                      (match_operand:V2HI 2 "permute_sel_operand" "r,i")
                     ] UNSPEC_VEC_PERM5)
   )
  ]
  "(TARGET_PULP_VECT_GAP8 && TARGET_PULP_VECT_SHUFFLEPACK)"
  "pv.pack.h.h \t%0,%2,%1 \t# Pack2 high"
[(set_attr "type" "move,move")
 (set_attr "mode" "SI,SI")]
)

/* __GAP8 Stop */

(define_expand "vec_permv2hi"
  [(match_operand:V2HI 0 "register_operand"    "")
   (match_operand:V2HI 1 "register_operand"    "")
   (match_operand:V2HI 2 "register_operand"    "")
   (match_operand:V2HI 3 "permute_sel_operand" "")
  ]
  "TARGET_PULP_VECT_SHUFFLEPACK"
{
	if (rtx_equal_p(operands[1], operands[2])) {
		emit_insn (gen_vec_permv2hi_internal2_1 (operands[0], operands[1], operands[2], operands[3]));
	} else {
		/* __GAP8 Start */
		/* balasr: since I patched this to depend on TARGET_PULP_VECT_GAP8 instead of Pulp_Cpu==Gap8, there might be some breakage */
                if (TARGET_PULP_VECT_GAP8 && (GET_CODE (operands[3]) == CONST_VECTOR) &&
                    (INTVAL(XVECEXP (operands[3], 0, 0)) == 0) && (INTVAL(XVECEXP (operands[3], 0, 1)) == 2)) {
                        emit_insn (gen_vec_permv2hi_low(operands[0], operands[1], operands[2]));
                } else if (TARGET_PULP_VECT_GAP8 && (GET_CODE (operands[3]) == CONST_VECTOR) &&
                           (INTVAL(XVECEXP (operands[3], 0, 0)) == 1) && (INTVAL(XVECEXP (operands[3], 0, 1)) == 3)) {
                        emit_insn (gen_vec_permv2hi_high(operands[0], operands[1], operands[2]));
                } else
		/* __GAP8 Stop */
		{
                        if (GET_CODE (operands[3]) != REG) operands[3] = force_reg (V2HImode, operands[3]);
                        emit_insn (gen_vec_permv2hi_internal2 (operands[0], operands[1], operands[2], operands[3]));
                }
	}
	DONE;
}
)

(define_insn "vec_permv4qi_internal2_1"
  [(set (match_operand:V4QI 0 "register_operand"               "=r,r")
        (unspec:V4QI [(match_operand:V4QI 1 "register_operand"  "r,r")
                      (match_operand:V4QI 2 "register_operand"  "1,1")
                      (match_operand:V4QI 3 "permute_sel_operand" "r,i")
		     ] UNSPEC_VEC_PERM2)
   )
  ]
  "TARGET_PULP_VECT_SHUFFLEPACK && riscv_valid_permute_operands (operands[1], operands[2], operands[3])"
{
	switch (which_alternative) {
		case 0:
			return "pv.shuffle.b\t%0,%1,%3";
		case 1:
			{
				int Mask=0;
				int Sel = INTVAL (XVECEXP (operands[3], 0, 3)) & 3;
				rtx xoperands[3];
				int i;

				xoperands[0] = operands[0]; xoperands[1] = operands[2];
  				for (i = 0; i < 3; ++i) Mask |= (((INTVAL (XVECEXP (operands[3], 0, i)) & 3))<<(2*i));
				xoperands[2] = gen_rtx_CONST_INT (SImode, Mask);
				switch (Sel) {
					case 0: output_asm_insn("pv.shuffleI0.sci.b\t%0,%1,%2", xoperands); break;
					case 1: output_asm_insn("pv.shuffleI1.sci.b\t%0,%1,%2", xoperands); break;
					case 2: output_asm_insn("pv.shuffleI2.sci.b\t%0,%1,%2", xoperands); break;
					case 3: output_asm_insn("pv.shuffleI3.sci.b\t%0,%1,%2", xoperands); break;
					default:;
				}
				return "";
			}
		default:
			return "";
	}
}
[(set_attr "type" "move,move")
 (set_attr "mode" "SI,SI")]
)

(define_insn "vec_permv4qi_internal2"
  [(set (match_operand:V4QI 0 "register_operand"               "=r")
        (unspec:V4QI [(match_operand:V4QI 1 "register_operand" "0")
                      (match_operand:V4QI 2 "register_operand" "r")
                      (match_operand:V4QI 3 "register_operand" "r")
		     ] UNSPEC_VEC_PERM3)
   )
  ]
  "TARGET_PULP_VECT_SHUFFLEPACK"
  "pv.shuffle2.b\t%0,%2,%3 \t# Shuffle2, bytes"
[(set_attr "type" "move")
 (set_attr "mode" "SI")]
)

(define_insn "vec_permv4qi_int1"
  [(set (match_operand:V4QI 0 "register_operand"               "=r,r")
        (unspec:V4QI [(match_operand:V4QI 1 "register_operand"  "r,r")
                      (match_operand:V4QI 2 "permute_sel_operand" "r,i")
		     ] UNSPEC_VEC_PERM1)
   )
  ]
  "TARGET_PULP_VECT_SHUFFLEPACK"
{
	switch (which_alternative) {
		case 0:
			return "pv.shuffle.b\t%0,%1,%2";
		case 1: {
				int Mask=0;
				int Sel = INTVAL (XVECEXP (operands[3], 0, 3)) & 3;
				rtx xoperands[3];
				int i;

				xoperands[0] = operands[0]; xoperands[1] = operands[1];
  				for (i = 0; i < 3; ++i) Mask |= (((INTVAL (XVECEXP (operands[2], 0, i)) & 3))<<(2*i));
				xoperands[2] = gen_rtx_CONST_INT (SImode, Mask);
				switch (Sel) {
					case 0: output_asm_insn("pv.shuffleI0.sci.b\t%0,%1,%2", xoperands); break;
					case 1: output_asm_insn("pv.shuffleI1.sci.b\t%0,%1,%2", xoperands); break;
					case 2: output_asm_insn("pv.shuffleI2.sci.b\t%0,%1,%2", xoperands); break;
					case 3: output_asm_insn("pv.shuffleI3.sci.b\t%0,%1,%2", xoperands); break;
					default:;
				}
				return "";
			}
		default:
			return "";
	}
}
[(set_attr "type" "move,move")
 (set_attr "mode" "SI,SI")]
)

(define_expand "vec_permv4qi"
  [(match_operand:V4QI 0 "register_operand" "")
   (match_operand:V4QI 1 "register_operand" "")
   (match_operand:V4QI 2 "register_operand" "")
   (match_operand:V4QI 3 "permute_sel_operand" "")
  ]
  "TARGET_PULP_VECT_SHUFFLEPACK"
{
	if (rtx_equal_p(operands[1], operands[2])) {
		emit_insn (gen_vec_permv4qi_internal2_1 (operands[0], operands[1], operands[2], operands[3]));
	} else {
		if (GET_CODE (operands[3]) != REG) operands[3] = force_reg (V4QImode, operands[3]);
		emit_insn (gen_vec_permv4qi_internal2 (operands[0], operands[1], operands[2], operands[3]));
	}

	DONE;
}
)

;; Vector Insert
(define_insn "vec_set<VMODEALL:mode>_internal"
  [(set (match_operand:VMODEALL 0 "register_operand" "=r,r")
        (vec_merge:VMODEALL
          (vec_duplicate:VMODEALL (match_operand:<vec_scalar_elmt> 1 "nonmemory_operand" "r,J"))
          (match_operand:VMODEALL 3 "register_operand" "0,0")
          (match_operand:SI 2 "immediate_operand" "i,i")))]
  "TARGET_PULP_VECT"
{
  int elt = ffs ((int) INTVAL (operands[2])) - 1;
  operands[2] = GEN_INT (elt);
  if (which_alternative == 0) return "pv.insert.<vec_size>\t%0,%1,%2\t # Vect insert";
  else return "pv.insert.<vec_size>\t%0,x0,%2\t # Vect insert 0";
}
[(set_attr "type" "move,move")
 (set_attr "mode" "SI,SI")]
)

(define_insn "vec_set_first<VMODEALL2:mode>_internal"
  [(set (match_operand:VMODEALL2 0 "register_operand" "=r,r")
        (vec_merge:VMODEALL2
          (vec_duplicate:VMODEALL2 (match_operand:<vec_scalar_elmt> 1 "nonmemory_operand" "r,J"))
	  (const_vector:VMODEALL2 [(const_int 0) (const_int 0)])
          (match_operand:SI 2 "const_1_operand" "Z,Z")))]
  "TARGET_PULP_VECT"
{
  int elt = ffs ((int) INTVAL (operands[2])) - 1;
  operands[2] = GEN_INT (elt);
  if (which_alternative == 0) {
	return "p.exthz \t%0,%1\t # Vect first insert half, pos 0";
  } else return "add\t%0,x0,%2\t # Vect first insert half 0, pos 0";
}
[(set_attr "type" "move,move")
 (set_attr "mode" "SI,SI")]
)

(define_insn "vec_set_first<VMODEALL4:mode>_internal"
  [(set (match_operand:VMODEALL4 0 "register_operand" "=r,r")
        (vec_merge:VMODEALL4
          (vec_duplicate:VMODEALL4 (match_operand:<vec_scalar_elmt> 1 "nonmemory_operand" "r,J"))
	  (const_vector:VMODEALL4 [(const_int 0) (const_int 0) (const_int 0) (const_int 0)])
          (match_operand:SI 2 "const_1_operand" "Z,Z")))]
 "TARGET_PULP_VECT"
{
  int elt = ffs ((int) INTVAL (operands[2])) - 1;

  operands[2] = GEN_INT (elt);
  if (which_alternative == 0) {
	return "and\t%0,%1,0xff\t # Vect first insert byte, pos 0";
  } else return "add\t%0,x0,%2\t # Vect first insert, pos 0";
}
[(set_attr "type" "move,move")
 (set_attr "mode" "SI,SI")]
)

(define_expand "vec_set_first<VMODEALL:mode>"
  [(match_operand:VMODEALL 0 "register_operand" "")
   (match_operand:<vec_scalar_elmt> 1 "nonmemory_operand" "")
   (match_operand:SI 2 "immediate_operand" "")]
  "TARGET_PULP_VECT"
{
  HOST_WIDE_INT elem = (HOST_WIDE_INT) 1 << INTVAL (operands[2]);	/* Should always be 1 */

  if ((GET_CODE (operands[1]) == CONST_INT) && (INTVAL(operands[1]) != 0)) {
	rtx Vect_Zero[4] = {const0_rtx, const0_rtx, const0_rtx, const0_rtx};
	rtx zero_vect = gen_rtx_CONST_VECTOR (<MODE>mode, gen_rtvec_v (GET_MODE_NUNITS(<MODE>mode), Vect_Zero));
	emit_insn(gen_mov<mode>_internal(operands[0], zero_vect));
  	emit_insn (gen_vec_set<mode>_internal (operands[0], operands[1], GEN_INT (elem), operands[0]));
  } else emit_insn (gen_vec_set_first<mode>_internal (operands[0], operands[1], GEN_INT (elem)));
  DONE;
})

(define_expand "vec_set<VMODEALL:mode>"
  [(match_operand:VMODEALL 0 "register_operand" "")
   (match_operand:<vec_scalar_elmt> 1 "nonmemory_operand" "")
   (match_operand:SI 2 "immediate_operand" "")]
  "TARGET_PULP_VECT"
{
  HOST_WIDE_INT elem = (HOST_WIDE_INT) 1 << INTVAL (operands[2]);
  emit_insn (gen_vec_set<mode>_internal (operands[0], operands[1], GEN_INT (elem), operands[0]));
  DONE;
})

;; Vector Extract

(define_insn "vec_extract_sext_<SUBDI:mode>_<VMODEALL:mode>"
  [(set (match_operand:SUBDI 0 "register_operand" "=r")
        (sign_extend:SUBDI
          (vec_select:<vec_scalar_elmt>
             (match_operand:VMODEALL 1 "register_operand" "r")
             (parallel [(match_operand:SI 2 "immediate_operand" "i")])
          )
        )
   )
  ]
  "TARGET_PULP_VECT"
  "pv.extract.<vec_size>\t%0,%1,%2\t # vect extract, with sign ext"
[(set_attr "type" "move")
 (set_attr "mode" "<SUBDI:MODE>")]
)

(define_insn "vec_extract_zext_<SUBDI:mode>_<VMODEALL:mode>"
  [(set (match_operand:SUBDI 0 "register_operand" "=r")
        (zero_extend:SUBDI
          (vec_select:<vec_scalar_elmt>
             (match_operand:VMODEALL 1 "register_operand" "r")
             (parallel [(match_operand:SI 2 "immediate_operand" "i")])
          )
        )
   )
  ]
  "TARGET_PULP_VECT"
  "pv.extractu.<vec_size>\t%0,%1,%2\t # vect extract, with zero ext"
[(set_attr "type" "move")
 (set_attr "mode" "<SUBDI:MODE>")]
)

(define_insn "vec_extract<VMODEALL:mode>"
  [(set (match_operand:<vec_scalar_elmt> 0 "register_operand" "=r")
        (vec_select:<vec_scalar_elmt>
           (match_operand:VMODEALL 1 "register_operand" "r")
           (parallel [(match_operand:SI 2 "immediate_operand" "i")])
        )
   )
  ]
  "TARGET_PULP_VECT"
  "pv.extract.<vec_size>\t%0,%1,%2\t # vect extract"
[(set_attr "type" "move")
 (set_attr "mode" "SI")]
)

;; Diadic Instructions
(define_code_iterator vec_op2      	[plus minus smin smax])
(define_code_iterator vec_op2u      	[umin umax])
(define_code_iterator vec_op2s     	[lshiftrt ashiftrt ashift])
(define_code_iterator vec_log2     	[and ior xor])
(define_code_attr vec_op2_name      	[(plus "add") (minus "sub") (smin "smin") (smax "smax") (mult "mul")])
(define_code_attr vec_op2u_name      	[(umin "umin") (umax "umax")])
(define_code_attr vec_op2s_name     	[(lshiftrt "vlshr") (ashiftrt "vashr") (ashift "vashl")])
(define_code_attr vec_log2_name    	[(and "and") (ior "ior") (xor "exor")])
(define_code_attr vec_op2_asm_name 	[(plus "add") (minus "sub") (smin "min") (smax "max") (mult "mult")])
(define_code_attr vec_op2u_asm_name 	[(umin "minu") (umax "maxu")])
(define_code_attr vec_op2s_asm_name 	[(lshiftrt "srl") (ashiftrt "sra") (ashift "sll")])
(define_code_attr vec_log2_asm_name 	[(and "and") (ior "or") (xor "xor")])

;;/* __GAP8 Start */

(define_insn "cplx_conjhi2"
 [(set (match_operand:V2HI 0 "register_operand" "=r")
       (vec_concat:V2HI
		(vec_select:HI (match_operand:V2HI 1 "register_operand" "r") (parallel [(const_int 0)]))
		(neg:HI (vec_select:HI (match_dup 1) (parallel [(const_int 1)])))
       )
  )
 ]
"(TARGET_PULP_VECT_GAP8 && TARGET_PULP_VECT)"
"pv.cplxconj.h \t%0,%1\t # Complex conjugate"
[(set_attr "type" "arith")
 (set_attr "mode" "SI")]
)

(define_insn "cplx_conjsi3"
 [(set (match_operand:V2HI 0 "register_operand" "=r")
       (vec_concat:V2HI
		(subreg:HI (match_operand:SI 1 "register_operand" "r") 0)
		(subreg:HI (neg:SI (match_operand:SI 2 "register_operand" "r")) 0)
       )
  )
 ]
"(TARGET_PULP_VECT_GAP8 && TARGET_PULP_VECT)"
"pv.cplxconj.h \t%0,%1\t # Complex conjugate, infered"
[(set_attr "type" "arith")
 (set_attr "mode" "SI")]
)

(define_insn "add_div2_v2hi3"
  [(set (match_operand:V2HI 0 "register_operand" "=r")
        (ashiftrt:V2HI
                (plus   (match_operand:V2HI 1 "register_operand" "r")
                        (match_operand:V2HI 2 "register_operand" "r")
                )
                (const_vector:V2HI [(const_int 1) (const_int 1)])
        )
   )
  ]
"(TARGET_PULP_VECT_GAP8 && TARGET_PULP_VECT)"
"pv.add.h.div2 \t%0,%1,%2\t # Add2>>1 Op Vect"
[(set_attr "type" "arith")
 (set_attr "mode" "SI")]
)

(define_insn "add_div2_v4qi3"
  [(set (match_operand:V4QI 0 "register_operand" "=r")
        (ashiftrt:V4QI
                (plus   (match_operand:V4QI 1 "register_operand" "r")
                        (match_operand:V4QI 2 "register_operand" "r")
                )
                (const_vector:V4QI [(const_int 1) (const_int 1) (const_int 1) (const_int 1)])
        )
   )
  ]
"(TARGET_PULP_VECT_GAP8 && TARGET_PULP_VECT)"
"pv.add.b.div2 \t%0,%1,%2\t # Add4>>1 Op Vect"
[(set_attr "type" "arith")
 (set_attr "mode" "SI")]
)

(define_insn "add_div4_v2hi3"
  [(set (match_operand:V2HI 0 "register_operand" "=r")
        (ashiftrt:V2HI
                (plus   (match_operand:V2HI 1 "register_operand" "r")
                        (match_operand:V2HI 2 "register_operand" "r")
                )
                (const_vector:V2HI [(const_int 2) (const_int 2)])
        )
   )
  ]
"(TARGET_PULP_VECT_GAP8 && TARGET_PULP_VECT)"
"pv.add.h.div4 \t%0,%1,%2\t # Add2>>2 Op Vect"
[(set_attr "type" "arith")
 (set_attr "mode" "SI")]
)

(define_insn "add_div4_v4qi3"
  [(set (match_operand:V4QI 0 "register_operand" "=r")
        (ashiftrt:V4QI
                (plus   (match_operand:V4QI 1 "register_operand" "r")
                        (match_operand:V4QI 2 "register_operand" "r")
                )
                (const_vector:V4QI [(const_int 2) (const_int 2) (const_int 2) (const_int 2)])
        )
   )
  ]
"(TARGET_PULP_VECT_GAP8 && TARGET_PULP_VECT)"
"pv.add.b.div4 \t%0,%1,%2\t # Add4>>2 Op Vect"
[(set_attr "type" "arith")
 (set_attr "mode" "SI")]
)

(define_insn "sub_div2_v2hi3"
  [(set (match_operand:V2HI 0 "register_operand" "=r")
        (ashiftrt:V2HI
                (minus  (match_operand:V2HI 1 "register_operand" "r")
                        (match_operand:V2HI 2 "register_operand" "r")
                )
                (const_vector:V2HI [(const_int 1) (const_int 1)])
        )
   )
  ]
"(TARGET_PULP_VECT_GAP8 && TARGET_PULP_VECT)"
"pv.sub.h.div2 \t%0,%1,%2\t # Sub2>>1 Op Vect"
[(set_attr "type" "arith")
 (set_attr "mode" "SI")]
)

(define_insn "sub_div2_v4qi3"
  [(set (match_operand:V4QI 0 "register_operand" "=r")
        (ashiftrt:V4QI
                (minus  (match_operand:V4QI 1 "register_operand" "r")
                        (match_operand:V4QI 2 "register_operand" "r")
                )
                (const_vector:V4QI [(const_int 1) (const_int 1) (const_int 1) (const_int 1)])
        )
   )
  ]
"(TARGET_PULP_VECT_GAP8 && TARGET_PULP_VECT)"
"pv.sub.b.div2 \t%0,%1,%2\t # Sub4>>1 Op Vect"
[(set_attr "type" "arith")
 (set_attr "mode" "SI")]
)

(define_insn "sub_div4_v2hi3"
  [(set (match_operand:V2HI 0 "register_operand" "=r")
        (ashiftrt:V2HI
                (minus  (match_operand:V2HI 1 "register_operand" "r")
                        (match_operand:V2HI 2 "register_operand" "r")
                )
                (const_vector:V2HI [(const_int 2) (const_int 2)])
        )
   )
  ]
"(TARGET_PULP_VECT_GAP8 && TARGET_PULP_VECT)"
"pv.sub.h.div4 \t%0,%1,%2\t # Sub2>>2 Op Vect"
[(set_attr "type" "arith")
 (set_attr "mode" "SI")]
)

(define_insn "sub_div4_v4qi3"
  [(set (match_operand:V4QI 0 "register_operand" "=r")
        (ashiftrt:V4QI
                (minus  (match_operand:V4QI 1 "register_operand" "r")
                        (match_operand:V4QI 2 "register_operand" "r")
                )
                (const_vector:V4QI [(const_int 2) (const_int 2) (const_int 2) (const_int 2)])
        )
   )
  ]
"(TARGET_PULP_VECT_GAP8 && TARGET_PULP_VECT)"
"pv.sub.b.div4 \t%0,%1,%2\t # Sub4>>2 Op Vect"
[(set_attr "type" "arith")
 (set_attr "mode" "SI")]
)

(define_insn "subrotmj_v2hi3"
  [(set (match_operand:V2HI 0 "register_operand" "=r")
        (vec_concat:V2HI
                (subreg:HI
                        (minus:SI
                                (sign_extend:SI (vec_select:HI (match_operand:V2HI 1 "register_operand" "r")
                                                               (parallel [(const_int 1)]))
                                )
                                (sign_extend:SI (vec_select:HI (match_operand:V2HI 2 "register_operand" "r")
                                                               (parallel [(const_int 1)]))
                                )
                        )
                        0
                )
                (subreg:HI
                        (minus:SI
                                (sign_extend:SI (vec_select:HI (match_dup 2) (parallel [(const_int 0)])))
                                (sign_extend:SI (vec_select:HI (match_dup 1) (parallel [(const_int 0)])))
                        )
                        0
                )
        )
   )
  ]
"(TARGET_PULP_VECT_GAP8 && TARGET_PULP_VECT)"
"pv.subrotmj.h \t%0,%1,%2"
[(set_attr "type" "arith")
 (set_attr "mode" "SI")]
)

;;/* __GAP8 Stop */

(define_insn "<vec_op2_name><VMODEALL:mode>3"
  [(set (match_operand:VMODEALL 0 "register_operand" "=r,r")
        (vec_op2:VMODEALL (match_operand:VMODEALL 1 "register_operand" "r,r")
                          (match_operand:VMODEALL 2 "nonmemory_operand" "r,vIsdc")
        )
   )
  ]
"TARGET_PULP_VECT"
"@
  pv.<vec_op2_asm_name>.<vec_size> \t%0,%1,%2\t # Vect Op Vect
  pv.<vec_op2_asm_name>.sci.<vec_size> \t%0,%1,%W2\t # Vect Op Immediate Scalar"
[(set_attr "type" "arith,arith")
 (set_attr "mode" "SI,SI")]
)

(define_insn "<vec_op2_name>sc<VMODEALL:mode>3"
  [(set (match_operand:VMODEALL 0 "register_operand" "=r")
        (vec_op2:VMODEALL (match_operand:VMODEALL 1 "register_operand" "r")
			  (vec_duplicate:VMODEALL (match_operand:<vec_scalar_elmt> 2 "register_operand" "r"))
	)
   )
  ]
"TARGET_PULP_VECT"
"pv.<vec_op2_asm_name>.sc.<vec_size> \t%0,%1,%2\t # Vect Op Scalar"
[(set_attr "type" "arith")
 (set_attr "mode" "SI")]
)

(define_insn "<vec_op2_name>_swap_sc<VMODEALL:mode>3"
  [(set (match_operand:VMODEALL 0 "register_operand" "=r")
        (vec_op2:VMODEALL (vec_duplicate:VMODEALL (match_operand:<vec_scalar_elmt> 1 "register_operand" "r"))
		          (match_operand:VMODEALL 2 "register_operand" "r")
	)
   )
  ]
"TARGET_PULP_VECT"
"pv.<vec_op2_asm_name>.sc.<vec_size> \t%0,%2,%1\t # Vect Op Scalar (swap)"
[(set_attr "type" "arith")
 (set_attr "mode" "SI")]
)


(define_insn "<vec_op2u_name><VMODEALL:mode>3"
  [(set (match_operand:VMODEALL 0 "register_operand" "=r,r")
        (vec_op2u:VMODEALL (match_operand:VMODEALL 1 "register_operand" "r,r")
                           (match_operand:VMODEALL 2 "nonmemory_operand" "r,vIusc")
        )
   )
  ]
"TARGET_PULP_VECT"
"@
  pv.<vec_op2u_asm_name>.<vec_size> \t%0,%1,%2\t # VectU Op Vect
  pv.<vec_op2u_asm_name>.sci.<vec_size> \t%0,%1,%w2\t # VectU Op Immediate Scalar"
[(set_attr "type" "arith,arith")
 (set_attr "mode" "SI,SI")]
)

(define_insn "<vec_op2u_name>sc<VMODEALL:mode>3"
  [(set (match_operand:VMODEALL 0 "register_operand" "=r")
        (vec_op2u:VMODEALL (match_operand:VMODEALL 1 "register_operand" "r")
			   (vec_duplicate:VMODEALL (match_operand:<vec_scalar_elmt> 2 "register_operand" "r"))
	)
   )
  ]
"TARGET_PULP_VECT"
"pv.<vec_op2u_asm_name>.sc.<vec_size> \t%0,%1,%2\t # VectU Op Scalar"
[(set_attr "type" "arith")
 (set_attr "mode" "SI")]
)

(define_insn "<vec_op2u_name>_swap_sc<VMODEALL:mode>3"
  [(set (match_operand:VMODEALL 0 "register_operand" "=r")
        (vec_op2u:VMODEALL (vec_duplicate:VMODEALL (match_operand:<vec_scalar_elmt> 1 "register_operand" "r"))
		           (match_operand:VMODEALL 2 "register_operand" "r")
	)
   )
  ]
"TARGET_PULP_VECT"
"pv.<vec_op2u_asm_name>.sc.<vec_size> \t%0,%2,%1\t # VectU Op Scalar (swap)"
[(set_attr "type" "arith")
 (set_attr "mode" "SI")]
)


(define_insn "<vec_op2s_name><VMODEALL:mode>3"
  [(set (match_operand:VMODEALL 0 "register_operand" "=r,r")
        (vec_op2s:VMODEALL (match_operand:VMODEALL 1 "register_operand" "r,r")
                           (match_operand:<VINT>   2 "nonmemory_operand" "r,vIsdc")
        )
   )
  ]
"TARGET_PULP_VECT"
"@
  pv.<vec_op2s_asm_name>.<vec_size> \t%0,%1,%2\t # Vect Shift Vect
  pv.<vec_op2s_asm_name>.sci.<vec_size> \t%0,%1,%W2\t # Vect Shift Immediate Scalar"
[(set_attr "type" "arith,arith")
 (set_attr "mode" "SI,SI")]
)

(define_insn "<vec_op2s_name>sc<VMODEALL:mode>3"
  [(set (match_operand:VMODEALL 0 "register_operand" "=r")
        (vec_op2s:VMODEALL (match_operand:<VINT>   1 "register_operand" "r")
			   (vec_duplicate:VMODEALL (match_operand:<vec_scalar_elmt> 2 "register_operand" "r"))
	)
   )
  ]
"TARGET_PULP_VECT"
"pv.<vec_op2s_asm_name>.sc.<vec_size> \t%0,%1,%2\t # Vect Shift Scalar"
[(set_attr "type" "arith")
 (set_attr "mode" "SI")]
)

(define_insn "avg<VMODEALL2:mode>3"
  [(set (match_operand:VMODEALL2 0 "register_operand" "=r,r")
	(ashiftrt:VMODEALL2
		(plus:VMODEALL2 (match_operand:VMODEALL2 1 "register_operand" "r,r")
			        (match_operand:VMODEALL2 2 "nonmemory_operand" "r,vIsdc"))
	   	(const_vector:VMODEALL2 [(const_int 1) (const_int 1)])
	)
   )
  ]
"TARGET_PULP_VECT"
"@
 pv.avg.<vec_size> \t%0,%1,%2\t # Vect Avg Vect
 pv.avg.sci.<vec_size> \t%0,%1,%W2\t # Vect Avg Scalar"
[(set_attr "type" "arith,arith")
 (set_attr "mode" "SI,SI")]
)


(define_insn "avg<VMODEALL4:mode>3"
  [(set (match_operand:VMODEALL4 0 "register_operand" "=r,r")
	(ashiftrt:VMODEALL4
		(plus:VMODEALL4 (match_operand:VMODEALL4 1 "register_operand" "r,r")
			        (match_operand:VMODEALL4 2 "nonmemory_operand" "r,vIsdc"))
	   	(const_vector:VMODEALL4 [(const_int 1) (const_int 1) (const_int 1) (const_int 1)])
	)
   )
  ]
"TARGET_PULP_VECT"
"@
 pv.avg.<vec_size> \t%0,%1,%2\t # Vect Avg Vect
 pv.avg.sci.<vec_size> \t%0,%1,%W2\t # Vect Avg Scalar"
[(set_attr "type" "arith,arith")
 (set_attr "mode" "SI,SI")]
)

(define_insn "avgsc<VMODEALL2:mode>3"
  [(set (match_operand:VMODEALL2 0 "register_operand" "=r")
	(ashiftrt:VMODEALL2
        	(plus:VMODEALL2 (match_operand:VMODEALL2 1 "register_operand" "r")
			        (vec_duplicate:VMODEALL2 (match_operand:<vec_scalar_elmt> 2 "register_operand" "r")))
	   	(const_vector:VMODEALL2 [(const_int 1) (const_int 1)])
	)
   )
  ]
"TARGET_PULP_VECT"
"pv.avg.sc.<vec_size> \t%0,%1,%2\t # Vect Avg Scalar"
[(set_attr "type" "arith")
 (set_attr "mode" "SI")]
)

(define_insn "avgsc<VMODEALL4:mode>3"
  [(set (match_operand:VMODEALL4 0 "register_operand" "=r")
	(ashiftrt:VMODEALL4
        	(plus:VMODEALL4 (match_operand:VMODEALL4 1 "register_operand" "r")
			        (vec_duplicate:VMODEALL4 (match_operand:<vec_scalar_elmt> 2 "register_operand" "r")))
	   	(const_vector:VMODEALL4 [(const_int 1) (const_int 1) (const_int 1) (const_int 1)])
	)
   )
  ]
"TARGET_PULP_VECT"
"pv.avg.sc.<vec_size> \t%0,%1,%2\t # Vect Avg Scalar"
[(set_attr "type" "arith")
 (set_attr "mode" "SI")]
)

(define_insn "avgsc_swap_<VMODEALL2:mode>3"
  [(set (match_operand:VMODEALL2 0 "register_operand" "=r")
	(ashiftrt:VMODEALL2
        	(plus:VMODEALL2 (vec_duplicate:VMODEALL2 (match_operand:<vec_scalar_elmt> 1 "register_operand" "r"))
		                (match_operand:VMODEALL2 2 "register_operand" "r"))
	   	(const_vector:VMODEALL2 [(const_int 1) (const_int 1)])
	)
   )
  ]
"TARGET_PULP_VECT"
"pv.avg.sc.<vec_size> \t%0,%2,%1\t # Vect Avg Scalar (swap)"
[(set_attr "type" "arith")
 (set_attr "mode" "SI")]
)

(define_insn "avgsc_swap_<VMODEALL4:mode>3"
  [(set (match_operand:VMODEALL4 0 "register_operand" "=r")
	(ashiftrt:VMODEALL4
        	(plus:VMODEALL4 (vec_duplicate:VMODEALL4 (match_operand:<vec_scalar_elmt> 1 "register_operand" "r"))
		                (match_operand:VMODEALL4 2 "register_operand" "r"))
	   	(const_vector:VMODEALL4 [(const_int 1) (const_int 1) (const_int 1) (const_int 1)])
	)
   )
  ]
"TARGET_PULP_VECT"
"pv.avg.sc.<vec_size> \t%0,%2,%1\t # Vect Avg Scalar (swap)"
[(set_attr "type" "arith")
 (set_attr "mode" "SI")]
)

;; Avg unsigned

(define_insn "avgv2uhi3"
  [(set (match_operand:V2HI 0 "register_operand" "=r,r")
	(lshiftrt:V2HI
		(plus:V2HI (match_operand:V2HI 1 "register_operand" "r,r")
			   (match_operand:V2HI 2 "nonmemory_operand" "r,vIusc"))
	   	(const_vector:V2HI [(const_int 1) (const_int 1)])
	)
   )
  ]
"TARGET_PULP_VECT"
"@
 pv.avgu.h \t%0,%1,%2\t # Vect2 Avgu Vect
 pv.avgu.sci.h \t%0,%1,%w2\t # Vect2 Avgu Scalar"
[(set_attr "type" "arith,arith")
 (set_attr "mode" "SI,SI")]
)

(define_insn "avgv4uqi3"
  [(set (match_operand:V4QI 0 "register_operand" "=r,r")
	(lshiftrt:V4QI
		(plus:V4QI (match_operand:V4QI 1 "register_operand" "r,r")
			   (match_operand:V4QI 2 "nonmemory_operand" "r,vIusc"))
	   	(const_vector:V4QI [(const_int 1) (const_int 1)(const_int 1)(const_int 1)])
	)
   )
  ]
"TARGET_PULP_VECT"
"@
 pv.avgu.b \t%0,%1,%2\t # Vect4 Avgu Vect
 pv.avgu.sci.b \t%0,%1,%w2\t # Vect4 Avgu Scalar"
[(set_attr "type" "arith,arith")
 (set_attr "mode" "SI,SI")]
)

(define_insn "avgscv2uhi3"
  [(set (match_operand:V2HI 0 "register_operand" "=r")
	(lshiftrt:V2HI
        	(plus:V2HI (match_operand:V2HI 1 "register_operand" "r")
			   (vec_duplicate:V2HI (match_operand:HI 2 "register_operand" "r")))
	   	(const_vector:V2HI [(const_int 1) (const_int 1)])
	)
   )
  ]
"TARGET_PULP_VECT"
"pv.avgu.sc.h \t%0,%1,%2\t # Vect 2 AvgU Scalar"
[(set_attr "type" "arith")
 (set_attr "mode" "SI")]
)

(define_insn "avgscv4uqi3"
  [(set (match_operand:V4QI 0 "register_operand" "=r")
	(lshiftrt:V4QI
        	(plus:V4QI (match_operand:V4QI 1 "register_operand" "r")
			   (vec_duplicate:V4QI (match_operand:QI 2 "register_operand" "r")))
	   	(const_vector:V4QI [(const_int 1) (const_int 1)(const_int 1)(const_int 1)])
	)
   )
  ]
"TARGET_PULP_VECT"
"pv.avgu.sc.b \t%0,%1,%2\t # Vect 4 AvgU Scalar"
[(set_attr "type" "arith")
 (set_attr "mode" "SI")]
)

(define_insn "avgsc_swap_v2uhi3"
  [(set (match_operand:V2HI 0 "register_operand" "=r")
	(lshiftrt:V2HI
        	(plus:V2HI (vec_duplicate:V2HI (match_operand:HI 1 "register_operand" "r"))
		           (match_operand:V2HI 2 "register_operand" "r"))
	   	(const_vector:V2HI [(const_int 1) (const_int 1)])
	)
   )
  ]
"TARGET_PULP_VECT"
"pv.avgu.sc.h \t%0,%2,%1\t # Vect 2 AvgU Scalar (swap)"
[(set_attr "type" "arith")
 (set_attr "mode" "SI")]
)

(define_insn "avgsc_swap_v4uqi3"
  [(set (match_operand:V4QI 0 "register_operand" "=r")
	(lshiftrt:V4QI
        	(plus:V4QI (vec_duplicate:V4QI (match_operand:QI 1 "register_operand" "r"))
		           (match_operand:V4QI 2 "register_operand" "r"))
	   	(const_vector:V4QI [(const_int 1) (const_int 1) (const_int 1) (const_int 1)])
	)
   )
  ]
"TARGET_PULP_VECT"
"pv.avgu.sc.b \t%0,%2,%1\t # Vect 4 AvgU Scalar (swap)"
[(set_attr "type" "arith")
 (set_attr "mode" "SI")]
)

;; Logical Instructions

(define_insn "<vec_log2_name><VMODEALL:mode>3"
  [(set (match_operand:VMODEALL 0 "register_operand" "=r,r")
        (vec_log2:VMODEALL (match_operand:VMODEALL 1 "register_operand" "r,r")
                           (match_operand:VMODEALL 2 "nonmemory_operand" "r,vIsdc")
        )
   )
  ]
"TARGET_PULP_VECT"
"@
  pv.<vec_log2_asm_name>.<vec_size> \t%0,%1,%2\t # Logical Vect Op Vect
  pv.<vec_log2_asm_name>.sci.<vec_size> \t%0,%1,%W2\t # Logical Vect Op Immediate Scalar"
[(set_attr "type" "logical,logical")
 (set_attr "mode" "SI,SI")]
)

(define_insn "<vec_log2_name>sc<VMODEALL:mode>3"
  [(set (match_operand:VMODEALL 0 "register_operand" "=r")
        (vec_log2:VMODEALL (match_operand:VMODEALL 1 "register_operand" "r")
			   (vec_duplicate:VMODEALL (match_operand:<vec_scalar_elmt> 2 "register_operand" "r"))
	)
   )
  ]
"TARGET_PULP_VECT"
"pv.<vec_log2_asm_name>.sc.<vec_size> \t%0,%1,%2\t # Logical Vect Op Scalar"
[(set_attr "type" "logical")
 (set_attr "mode" "SI")]
)

(define_insn "<vec_log2_name>_swap_sc<VMODEALL:mode>3"
  [(set (match_operand:VMODEALL 0 "register_operand" "=r")
        (vec_log2:VMODEALL (vec_duplicate:VMODEALL (match_operand:<vec_scalar_elmt> 1 "register_operand" "r"))
		           (match_operand:VMODEALL 2 "register_operand" "r")
	)
   )
  ]
"TARGET_PULP_VECT"
"pv.<vec_log2_asm_name>.sc.<vec_size> \t%0,%2,%1\t # Logical Vect Op Scalar (swap)"
[(set_attr "type" "logical")
 (set_attr "mode" "SI")]
)

;; Unary instructions

(define_insn "abs<VMODEALL:mode>2"
  [(set (match_operand:VMODEALL 0 "register_operand" "=r")
        (abs:VMODEALL (match_operand:VMODEALL 1 "register_operand" "r")
        )
   )
  ]
"TARGET_PULP_VECT"
"pv.abs.<vec_size> \t%0,%1\t # Vect abs"
[(set_attr "type" "arith")
 (set_attr "mode" "SI")]
)

(define_insn "neg<VMODEALL:mode>2"
  [(set (match_operand:VMODEALL 0 "register_operand" "=r")
        (neg:VMODEALL (match_operand:VMODEALL 1 "register_operand" "r")
        )
   )
  ]
"TARGET_PULP_VECT"
"pv.sub.<vec_size> \t%0,x0,%1\t # Vect neg"
[(set_attr "type" "arith")
 (set_attr "mode" "SI")]
)

/* __GAP8 Start */
;; Complex product

(define_insn "cplxmulsv2hi"
  [(set (match_operand:V2HI 0 "register_operand" "=r,r")
	(vec_concat:V2HI
		(subreg:HI
			(ashiftrt:SI
				(minus:SI
					(mult:SI
						(sign_extend:SI
							(vec_select:HI (match_operand:V2HI 1 "register_operand" "r,r") (parallel [(const_int 0)]))
						)
						(sign_extend:SI
							(vec_select:HI (match_operand:V2HI 2 "nonmemory_operand" "r,vIsdc") (parallel [(const_int 0)]))
						)
					)
					(mult:SI
						(sign_extend:SI (vec_select:HI (match_dup 1) (parallel [(const_int 1)])))
						(sign_extend:SI (vec_select:HI (match_dup 2) (parallel [(const_int 1)])))
					)
				)
				(const_int 15)
			) 0
		)
		(subreg:HI
			(ashiftrt:SI
				(plus:SI (mult:SI
						(sign_extend:SI (vec_select:HI (match_dup 1) (parallel [(const_int 0)])))
						(sign_extend:SI (vec_select:HI (match_dup 2) (parallel [(const_int 1)])))
					 )
					 (mult:SI
						(sign_extend:SI (vec_select:HI (match_dup 1) (parallel [(const_int 1)])))
						(sign_extend:SI (vec_select:HI (match_dup 2) (parallel [(const_int 0)])))
					 )
				)
				(const_int 15)
			) 0
		)
	)
   )
  ]
"(TARGET_PULP_VECT_GAP8 && TARGET_PULP_VECT)"
"@
 pv.cplxmul.s \t%0,%1,%2\t # Vect/Vect Cplx signed multiply
 pv.cplxmul.sci.s \t%0,%1,%W2\t # Vect/ScalImm Cplx signed multiply"
[(set_attr "type" "arith,arith")
 (set_attr "mode" "SI,SI")]
)

(define_insn "cplxmulsv2hi_div2"
  [(set (match_operand:V2HI 0 "register_operand" "=r")
        (vec_concat:V2HI
                (subreg:HI
                        (ashiftrt:SI
                                (minus:SI
                                        (mult:SI
                                                (sign_extend:SI
                                                        (vec_select:HI (match_operand:V2HI 1 "register_operand" "r") (parallel [(const_int 0)]))
                                                )
                                                (sign_extend:SI
                                                        (vec_select:HI (match_operand:V2HI 2 "register_operand" "r") (parallel [(const_int 0)]))
                                                )
                                        )
                                        (mult:SI
                                                (sign_extend:SI (vec_select:HI (match_dup 1) (parallel [(const_int 1)])))
                                                (sign_extend:SI (vec_select:HI (match_dup 2) (parallel [(const_int 1)])))
                                        )
                                )
                                (const_int 16)
                        ) 0
                )
                (subreg:HI
                        (ashiftrt:SI
                                (plus:SI (mult:SI
                                                (sign_extend:SI (vec_select:HI (match_dup 1) (parallel [(const_int 0)])))
                                                (sign_extend:SI (vec_select:HI (match_dup 2) (parallel [(const_int 1)])))
                                         )
                                         (mult:SI
                                                (sign_extend:SI (vec_select:HI (match_dup 1) (parallel [(const_int 1)])))
                                                (sign_extend:SI (vec_select:HI (match_dup 2) (parallel [(const_int 0)])))
                                         )
                                )
                                (const_int 16)
                        ) 0
                )
        )
   )
  ]
"(TARGET_PULP_VECT_GAP8 && TARGET_PULP_VECT)"
"pv.cplxmul.s.div2 \t%0,%1,%2\t # Q15 Vect/Vect Cplx signed multiply >> 1"
[(set_attr "type" "arith")
 (set_attr "mode" "SI")]
)

(define_insn "cplxmulsv2hi_div4"
  [(set (match_operand:V2HI 0 "register_operand" "=r")
        (vec_concat:V2HI
                (subreg:HI
                        (ashiftrt:SI
                                (minus:SI
                                        (mult:SI
                                                (sign_extend:SI
                                                        (vec_select:HI (match_operand:V2HI 1 "register_operand" "r") (parallel [(const_int 0)]))
                                                )
                                                (sign_extend:SI
                                                        (vec_select:HI (match_operand:V2HI 2 "register_operand" "r") (parallel [(const_int 0)]))
                                                )
                                        )
                                        (mult:SI
                                                (sign_extend:SI (vec_select:HI (match_dup 1) (parallel [(const_int 1)])))
                                                (sign_extend:SI (vec_select:HI (match_dup 2) (parallel [(const_int 1)])))
                                        )
                                )
                                (const_int 17)
                        ) 0
                )
                (subreg:HI
                        (ashiftrt:SI
                                (plus:SI (mult:SI
                                                (sign_extend:SI (vec_select:HI (match_dup 1) (parallel [(const_int 0)])))
                                                (sign_extend:SI (vec_select:HI (match_dup 2) (parallel [(const_int 1)])))
                                         )
                                         (mult:SI
                                                (sign_extend:SI (vec_select:HI (match_dup 1) (parallel [(const_int 1)])))
                                                (sign_extend:SI (vec_select:HI (match_dup 2) (parallel [(const_int 0)])))
                                         )
                                )
                                (const_int 17)
                        ) 0
                )
        )
   )
  ]
"(TARGET_PULP_VECT_GAP8 && TARGET_PULP_VECT)"
"pv.cplxmul.s.div4 \t%0,%1,%2\t # Q15 Vect/Vect Cplx signed multiply >> 2"
[(set_attr "type" "arith")
 (set_attr "mode" "SI")]
)

;; Viterbi Max

(define_insn "vec_vit_max_v2hi"
  [(set (match_operand:V2HI 0 "register_operand"               "=r")
        (unspec:V2HI [(match_operand:V2HI 1 "register_operand" "r")
                      (match_operand:V2HI 2 "register_operand" "r")
                     ] UNSPEC_VIT_MAX)
   )
   (clobber (reg:SI VIT_REG))
  ]
  "(TARGET_PULP_VECT_GAP8 && TARGET_PULP_VECT_SHUFFLEPACK)"
  "pv.vitop.max \t%0,%1,%2\t # Vect 2 Viterbi max"
[(set_attr "type" "arith")
 (set_attr "mode" "SI")]
)

;; Viterbi Select

(define_insn "vec_vit_sel_v2hi"
  [(set (match_operand:V2HI 0 "register_operand"               "=r")
        (unspec:V2HI [(match_operand:V2HI 1 "register_operand" "r")
                      (match_operand:V2HI 2 "register_operand" "r")
                      (reg:SI VIT_REG)
                     ] UNSPEC_VIT_SEL)
   )
  ]
  "(TARGET_PULP_VECT_GAP8 && TARGET_PULP_VECT_SHUFFLEPACK)"
  "pv.vitop.sel \t%0,%1,%2\t # Vect 2 Viterbi select"
[(set_attr "type" "arith")
 (set_attr "mode" "SI")]
)


;; Simple dot products

(define_insn "dotpv2hi"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
	(plus:SI
		(mult:SI
			(sign_extend:SI (vec_select:HI (match_operand:V2HI 1 "register_operand" "r,r") (parallel [(const_int 0)])))
			(sign_extend:SI (vec_select:HI (match_operand:V2HI 2 "nonmemory_operand" "r,vIsdc") (parallel [(const_int 0)])))
		)
		(mult:SI (sign_extend:SI (vec_select:HI (match_dup 1) (parallel [(const_int 1)])))
			 (sign_extend:SI (vec_select:HI (match_dup 2) (parallel [(const_int 1)])))
		)
	)
   )
  ]
"TARGET_PULP_VECT"
"@
  pv.dotsp.h \t%0,%1,%2\t # Vect 2 dot product
  pv.dotsp.sci.h \t%0,%1,%W2\t # Vect/Imm 2 dot product"
[(set_attr "type" "arith,arith")
 (set_attr "mode" "SI,SI")]
)

(define_insn "dotspscv2hi_le"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(plus:SI
		(mult:SI
			(sign_extend:SI (vec_select:HI (match_operand:V2HI 1 "register_operand" "r") (parallel [(const_int 0)])))
			(sign_extend:SI (subreg:HI (match_operand:SI 2 "register_operand" "r") 0))
		)
		(mult:SI
			(sign_extend:SI (vec_select:HI (match_dup 1) (parallel [(const_int 1)])))
			(sign_extend:SI (subreg:HI (match_dup 2) 0))
		)
	)
   )
  ]
"TARGET_PULP_VECT"
"pv.dotsp.sc.h \t%0,%1,%2\t # Vect/Scalar reg 2 signed dot product"
[(set_attr "type" "arith")
 (set_attr "mode" "SI")]
)

(define_insn "reduc_plus_scal_v2hi"
  [(set (match_operand:HI 0 "register_operand" "=r")
	(plus:HI (vec_select:HI (match_operand:V2HI 1 "register_operand" "r") (parallel [(const_int 0)]))
		 (vec_select:HI (match_dup 1) (parallel [(const_int 1)]))
	)
   )
  ]
"TARGET_PULP_VECT"
"pv.dotsp.sci.h \t%0,%1,1\t # Vect 2 Sum of elements (reduction)"
[(set_attr "type" "arith")
 (set_attr "mode" "SI")]
)

(define_insn "dotupv2hi"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
	(plus:SI
		(mult:SI
			(zero_extend:SI (vec_select:HI (match_operand:V2HI 1 "register_operand" "r,r") (parallel [(const_int 0)])))
			(zero_extend:SI (vec_select:HI (match_operand:V2HI 2 "nonmemory_operand" "r,vIusc") (parallel [(const_int 0)])))
		)
		(mult:SI
			(zero_extend:SI (vec_select:HI (match_dup 1) (parallel [(const_int 1)])))
			(zero_extend:SI (vec_select:HI (match_dup 2) (parallel [(const_int 1)])))
		)
	)
   )
  ]
"TARGET_PULP_VECT"
"@
 pv.dotup.h \t%0,%1,%2\t # Vect 2 unsigned dot product
 pv.dotup.sci.h \t%0,%1,%w2\t # Vect/Imm 2 unsigned dot product"
[(set_attr "type" "arith,arith")
 (set_attr "mode" "SI,SI")]
)

(define_insn "dotupscv2hi_le"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(plus:SI
		(mult:SI
			(zero_extend:SI (vec_select:HI (match_operand:V2HI 1 "register_operand" "r") (parallel [(const_int 0)])))
			(zero_extend:SI (subreg:HI (match_operand:SI 2 "register_operand" "r") 0))
		)
		(mult:SI
			(zero_extend:SI (vec_select:HI (match_dup 1) (parallel [(const_int 1)])))
			(zero_extend:SI (subreg:HI (match_dup 2) 0))
		)
	)
   )
  ]
"TARGET_PULP_VECT"
"pv.dotup.sc.h \t%0,%1,%2\t # Vect/Scalar reg 2 unssigned dot product"
[(set_attr "type" "arith")
 (set_attr "mode" "SI")]
)

(define_insn "dotuspv2hi"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
	(plus:SI
		(mult:SI
			(zero_extend:SI (vec_select:HI (match_operand:V2HI 1 "register_operand" "r,r") (parallel [(const_int 0)])))
			(sign_extend:SI (vec_select:HI (match_operand:V2HI 2 "nonmemory_operand" "r,vIsdc") (parallel [(const_int 0)])))
		)
		(mult:SI
			(zero_extend:SI (vec_select:HI (match_dup 1) (parallel [(const_int 1)])))
			(sign_extend:SI (vec_select:HI (match_dup 2) (parallel [(const_int 1)])))
		)
	)
   )
  ]
"TARGET_PULP_VECT"
"@
 pv.dotusp.h \t%0,%1,%2\t # Vect 2 unsigned/signed dot product
 pv.dotusp.sci.h \t%0,%1,%W2\t # Vect/Imm 2 unsigned/signed dot product"
[(set_attr "type" "arith,arith")
 (set_attr "mode" "SI,SI")]
)

(define_insn "dotuspscv2hi_le"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(plus:SI
		(mult:SI
			(zero_extend:SI (vec_select:HI (match_operand:V2HI 1 "register_operand" "r") (parallel [(const_int 0)])))
			(sign_extend:SI (subreg:HI (match_operand:SI 2 "register_operand" "r") 0))
		)
		(mult:SI
			(zero_extend:SI (vec_select:HI (match_dup 1) (parallel [(const_int 1)])))
			(sign_extend:SI (subreg:HI (match_dup 2) 0))
		)
	)
   )
  ]
"TARGET_PULP_VECT"
"pv.dotusp.sc.h \t%0,%1,%2\t # Vect/Scalar reg 2 unsigned/signed dot product"
[(set_attr "type" "arith")
 (set_attr "mode" "SI")]
)

(define_insn "dotpv4qi"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
	(plus:SI
		(plus:SI
			(mult:SI
				(sign_extend:SI (vec_select:QI (match_operand:V4QI 1 "register_operand" "r,r") (parallel [(const_int 0)])))
				(sign_extend:SI (vec_select:QI (match_operand:V4QI 2 "nonmemory_operand" "r,vIsdc") (parallel [(const_int 0)])))
			)
			(mult:SI
				(sign_extend:SI (vec_select:QI (match_dup 1) (parallel [(const_int 1)])))
				(sign_extend:SI (vec_select:QI (match_dup 2) (parallel [(const_int 1)])))
			)
		)
		(plus:SI
			(mult:SI
				(sign_extend:SI (vec_select:QI (match_dup 1) (parallel [(const_int 2)])))
				(sign_extend:SI (vec_select:QI (match_dup 2) (parallel [(const_int 2)])))
			)
			(mult:SI
				(sign_extend:SI (vec_select:QI (match_dup 1) (parallel [(const_int 3)])))
				(sign_extend:SI (vec_select:QI (match_dup 2) (parallel [(const_int 3)])))
			)
		)
	)
   )
  ]
"TARGET_PULP_VECT"
"@
 pv.dotsp.b \t%0,%1,%2\t # Vect 4 dot product
 pv.dotsp.sci.b \t%0,%1,%W2\t # Vect/Imm 4 dot product"
[(set_attr "type" "arith,arith")
 (set_attr "mode" "SI,SI")]
)

(define_insn "reduc_plus_scal_v4qi"
  [(set (match_operand:QI 0 "register_operand" "=r")
	(plus:QI
		(plus:QI (vec_select:QI (match_operand:V4QI 1 "register_operand" "r") (parallel [(const_int 0)]))
			 (vec_select:QI (match_dup 1) (parallel [(const_int 1)]))
		)
		(plus:QI (vec_select:QI (match_dup 1) (parallel [(const_int 2)]))
			 (vec_select:QI (match_dup 1) (parallel [(const_int 3)]))
		)
	)
   )
  ]
"TARGET_PULP_VECT"
"pv.dotsp.sci.b \t%0,%1,1\t # Vect 4 sum of elements (reduction)"
[(set_attr "type" "arith")
 (set_attr "mode" "SI")]
)

(define_insn "dotspscv4qi_le"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(plus:SI
		(plus:SI
			(mult:SI
				(sign_extend:SI (vec_select:QI (match_operand:V4QI 1 "register_operand" "r") (parallel [(const_int 0)])))
				(sign_extend:SI (subreg:QI (match_operand:SI 2 "register_operand" "r") 0))
			)
			(mult:SI
				(sign_extend:SI (vec_select:QI (match_dup 1) (parallel [(const_int 1)])))
				(sign_extend:SI (subreg:QI (match_dup 2) 0))
			)
		)
		(plus:SI
			(mult:SI
				(sign_extend:SI (vec_select:QI (match_dup 1) (parallel [(const_int 2)])))
				(sign_extend:SI (subreg:QI (match_dup 2) 0))
			)
			(mult:SI
				(sign_extend:SI (vec_select:QI (match_dup 1) (parallel [(const_int 3)])))
				(sign_extend:SI (subreg:QI (match_dup 2) 0))
			)
		)
	)
   )
  ]
"TARGET_PULP_VECT"
"pv.dotsp.sc.b \t%0,%1,%2\t # Vect/Scalar reg 4 dot product"
[(set_attr "type" "arith")
 (set_attr "mode" "SI")]
)

(define_insn "dotupv4qi"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
	(plus:SI
		(plus:SI
			(mult:SI
				(zero_extend:SI (vec_select:QI (match_operand:V4QI 1 "register_operand" "r,r") (parallel [(const_int 0)])))
				(zero_extend:SI (vec_select:QI (match_operand:V4QI 2 "nonmemory_operand" "r,vIusc") (parallel [(const_int 0)])))
			)
			(mult:SI
				(zero_extend:SI (vec_select:QI (match_dup 1) (parallel [(const_int 1)])))
				(zero_extend:SI (vec_select:QI (match_dup 2) (parallel [(const_int 1)])))
			)
		)
		(plus:SI
			(mult:SI
				(zero_extend:SI (vec_select:QI (match_dup 1) (parallel [(const_int 2)])))
				(zero_extend:SI (vec_select:QI (match_dup 2) (parallel [(const_int 2)])))
			)
			(mult:SI
				(zero_extend:SI (vec_select:QI (match_dup 1) (parallel [(const_int 3)])))
				(zero_extend:SI (vec_select:QI (match_dup 2) (parallel [(const_int 3)])))
			)
		)
	)
   )
  ]
"TARGET_PULP_VECT"
"@
 pv.dotup.b \t%0,%1,%2\t # Vect 4 unsigned dot product
 pv.dotup.sci.b \t%0,%1,%w2\t # Vect/Imm 4 unsigned dot product"
[(set_attr "type" "arith,arith")
 (set_attr "mode" "SI,SI")]
)

(define_insn "dotupscv4qi_le"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(plus:SI
		(plus:SI
			(mult:SI
				(zero_extend:SI (vec_select:QI (match_operand:V4QI 1 "register_operand" "r") (parallel [(const_int 0)])))
				(zero_extend:SI (subreg:QI (match_operand:SI 2 "register_operand" "r") 0))
			)
			(mult:SI
				(zero_extend:SI (vec_select:QI (match_dup 1) (parallel [(const_int 1)])))
				(zero_extend:SI (subreg:QI (match_dup 2) 0))
			)
		)
		(plus:SI
			(mult:SI
				(zero_extend:SI (vec_select:QI (match_dup 1) (parallel [(const_int 2)])))
				(zero_extend:SI (subreg:QI (match_dup 2) 0))
			)
			(mult:SI
				(zero_extend:SI (vec_select:QI (match_dup 1) (parallel [(const_int 3)])))
				(zero_extend:SI (subreg:QI (match_dup 2) 0))
			)
		)
	)
   )
  ]
"TARGET_PULP_VECT"
"pv.dotup.sc.b \t%0,%1,%2\t # Vect/Scalar reg 4 unsigned dot product"
[(set_attr "type" "arith")
 (set_attr "mode" "SI")]
)

(define_insn "dotuspv4qi"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
	(plus:SI
		(plus:SI
			(mult:SI
				(zero_extend:SI (vec_select:QI (match_operand:V4QI 1 "register_operand" "r,r") (parallel [(const_int 0)])))
				(sign_extend:SI (vec_select:QI (match_operand:V4QI 2 "nonmemory_operand" "r,vIsdc") (parallel [(const_int 0)])))
			)
			(mult:SI
				(zero_extend:SI (vec_select:QI (match_dup 1) (parallel [(const_int 1)])))
				(sign_extend:SI (vec_select:QI (match_dup 2) (parallel [(const_int 1)])))
			)
		)
		(plus:SI
			(mult:SI
				(zero_extend:SI (vec_select:QI (match_dup 1) (parallel [(const_int 2)])))
				(sign_extend:SI (vec_select:QI (match_dup 2) (parallel [(const_int 2)])))
			)
			(mult:SI
				(zero_extend:SI (vec_select:QI (match_dup 1) (parallel [(const_int 3)])))
				(sign_extend:SI (vec_select:QI (match_dup 2) (parallel [(const_int 3)])))
			)
		)
	)
   )
  ]
"TARGET_PULP_VECT"
"@
 pv.dotusp.b \t%0,%1,%2\t # Vect 4 unsigned/signed dot product
 pv.dotusp.sci.b \t%0,%1,%W2\t # Vect/Imm 4 unsigned/signed dot product"
[(set_attr "type" "arith,arith")
 (set_attr "mode" "SI,SI")]
)

(define_insn "dotuspscv4qi_le"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(plus:SI
		(plus:SI
			(mult:SI
				(zero_extend:SI (vec_select:QI (match_operand:V4QI 1 "register_operand" "r") (parallel [(const_int 0)])))
				(sign_extend:SI (subreg:QI (match_operand:SI 2 "register_operand" "r") 0))
			)
			(mult:SI
				(zero_extend:SI (vec_select:QI (match_dup 1) (parallel [(const_int 1)])))
				(sign_extend:SI (subreg:QI (match_dup 2) 0))
			)
		)
		(plus:SI
			(mult:SI
				(zero_extend:SI (vec_select:QI (match_dup 1) (parallel [(const_int 2)])))
				(sign_extend:SI (subreg:QI (match_dup 2) 0))
			)
			(mult:SI
				(zero_extend:SI (vec_select:QI (match_dup 1) (parallel [(const_int 3)])))
				(sign_extend:SI (subreg:QI (match_dup 2) 0))
			)
		)
	)
   )
  ]
"TARGET_PULP_VECT"
"pv.dotusp.sc.b \t%0,%1,%2\t # Vect/Scalar reg 4 unsigned/signed dot product"
[(set_attr "type" "arith")
 (set_attr "mode" "SI")]
)

;; Dot products with accumulation

(define_insn "sdot_prodv2hi"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
	(plus:SI
		(plus:SI
			(mult:SI
				(sign_extend:SI (vec_select:HI (match_operand:V2HI 1 "register_operand" "r,r") (parallel [(const_int 0)])))
				(sign_extend:SI (vec_select:HI (match_operand:V2HI 2 "nonmemory_operand" "r,vIsdc") (parallel [(const_int 0)])))
			)
			(mult:SI
				(sign_extend:SI (vec_select:HI (match_dup 1) (parallel [(const_int 1)])))
				(sign_extend:SI (vec_select:HI (match_dup 2) (parallel [(const_int 1)])))
			)
		)
		(match_operand:SI 3 "register_operand" "0,0")
	)
   )
  ]
"TARGET_PULP_VECT"
"@
 pv.sdotsp.h \t%0,%1,%2\t # Accumulation of 2 half dot products Vect/Vect
 pv.sdotsp.sci.h \t%0,%1,%W2\t # Accumulation of 2 half dot products vect/Imm"
[(set_attr "type" "arith,arith")
 (set_attr "mode" "SI,SI")]
)

(define_insn "sdotspscv2hi_le"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(plus:SI
		(plus:SI
			(mult:SI
				(sign_extend:SI (vec_select:HI (match_operand:V2HI 1 "register_operand" "r") (parallel [(const_int 0)])))
				(sign_extend:SI (subreg:HI (match_operand:SI 2 "register_operand" "r") 0))
			)
			(mult:SI
				(sign_extend:SI (vec_select:HI (match_dup 1) (parallel [(const_int 1)])))
				(sign_extend:SI (subreg:HI (match_dup 2) 0))
			)
		)
		(match_operand:SI 3 "register_operand" "0")
	)
   )
  ]
"TARGET_PULP_VECT"
"pv.sdotsp.sc.h \t%0,%1,%2\t # Accumulation of Vect/Scalar reg 2 signed dot product"
[(set_attr "type" "arith")
 (set_attr "mode" "SI")]
)

(define_insn "udot_prodv2hi"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
	(plus:SI
		(plus:SI
			(mult:SI
				(zero_extend:SI (vec_select:HI (match_operand:V2HI 1 "register_operand" "r,r") (parallel [(const_int 0)])))
				(zero_extend:SI (vec_select:HI (match_operand:V2HI 2 "nonmemory_operand" "r,vIusc") (parallel [(const_int 0)])))
			)
			(mult:SI
				(zero_extend:SI (vec_select:HI (match_dup 1) (parallel [(const_int 1)])))
				(zero_extend:SI (vec_select:HI (match_dup 2) (parallel [(const_int 1)])))
			)
		)
		(match_operand:SI 3 "register_operand" "0,0")
	)
   )
  ]
"TARGET_PULP_VECT"
"@
 pv.sdotup.h \t%0,%1,%2\t # Accumulation of 2 half unsigned dot products Vect/Vect
 pv.sdotup.sci.h \t%0,%1,%w2\t # Accumulation of 2 half unsigned dot products Vect/Imm"
[(set_attr "type" "arith,arith")
 (set_attr "mode" "SI,SI")]
)

(define_insn "sdotupscv2hi_le"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(plus:SI
		(plus:SI
			(mult:SI
				(zero_extend:SI (vec_select:HI (match_operand:V2HI 1 "register_operand" "r") (parallel [(const_int 0)])))
				(zero_extend:SI (subreg:HI (match_operand:SI 2 "register_operand" "r") 0))
			)
			(mult:SI
				(zero_extend:SI (vec_select:HI (match_dup 1) (parallel [(const_int 1)])))
				(zero_extend:SI (subreg:HI (match_dup 2) 0))
			)
		)
		(match_operand:SI 3 "register_operand" "0")
	)
   )
  ]
"TARGET_PULP_VECT"
"pv.sdotup.sc.h \t%0,%1,%2\t # Accumulation of Vect/Scalar reg 2 unsigned dot product"
[(set_attr "type" "arith")
 (set_attr "mode" "SI")]
)

(define_insn "sdotuspv2hi"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
	(plus:SI
		(plus:SI
			(mult:SI
				(zero_extend:SI (vec_select:HI (match_operand:V2HI 1 "register_operand" "r,r") (parallel [(const_int 0)])))
				(sign_extend:SI (vec_select:HI (match_operand:V2HI 2 "nonmemory_operand" "r,vIusc") (parallel [(const_int 0)])))
			)
			(mult:SI
				(zero_extend:SI (vec_select:HI (match_dup 1) (parallel [(const_int 1)])))
				(sign_extend:SI (vec_select:HI (match_dup 2) (parallel [(const_int 1)])))
			)
		)
		(match_operand:SI 3 "register_operand" "0,0")
	)
   )
  ]
"TARGET_PULP_VECT"
"@
 pv.sdotusp.h \t%0,%1,%2\t # Accumulation of 2 half unsigned/signed dot products Vect/Vect
 pv.sdotusp.sci.h \t%0,%1,%W2\t # Accumulation of 2 half unsigned/signed dot products Vect/Imm"
[(set_attr "type" "arith,arith")
 (set_attr "mode" "SI,SI")]
)

(define_insn "sdotuspscv2hi_le"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(plus:SI
		(plus:SI
			(mult:SI
				(zero_extend:SI (vec_select:HI (match_operand:V2HI 1 "register_operand" "r") (parallel [(const_int 0)])))
				(sign_extend:SI (subreg:HI (match_operand:SI 2 "register_operand" "r") 0))
			)
			(mult:SI
				(zero_extend:SI (vec_select:HI (match_dup 1) (parallel [(const_int 1)])))
				(sign_extend:SI (subreg:HI (match_dup 2) 0))
			)
		)
		(match_operand:SI 3 "register_operand" "0")
	)
   )
  ]
"TARGET_PULP_VECT"
"pv.sdotusp.sc.h \t%0,%1,%2\t # Accumulation of Vect/Scalar reg 2 unsigned/signed dot product"
[(set_attr "type" "arith")
 (set_attr "mode" "SI")]
)

(define_insn "sdot_prodv4qi"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
	(plus:SI
		(plus:SI
			(plus:SI
				(mult:SI
					(sign_extend:SI (vec_select:QI (match_operand:V4QI 1 "register_operand" "r,r") (parallel [(const_int 0)])))
					(sign_extend:SI (vec_select:QI (match_operand:V4QI 2 "nonmemory_operand" "r,vIsdc") (parallel [(const_int 0)])))
				)
				(mult:SI
					(sign_extend:SI (vec_select:QI (match_dup 1) (parallel [(const_int 1)])))
					(sign_extend:SI (vec_select:QI (match_dup 2) (parallel [(const_int 1)])))
				)
			)
			(plus:SI
				(mult:SI
					(sign_extend:SI (vec_select:QI (match_dup 1) (parallel [(const_int 2)])))
					(sign_extend:SI (vec_select:QI (match_dup 2) (parallel [(const_int 2)])))
				)
				(mult:SI
					(sign_extend:SI (vec_select:QI (match_dup 1) (parallel [(const_int 3)])))
					(sign_extend:SI (vec_select:QI (match_dup 2) (parallel [(const_int 3)])))
				)
			)
		)
		(match_operand:SI 3 "register_operand" "0,0")
	)
   )
  ]
"TARGET_PULP_VECT"
"@
 pv.sdotsp.b \t%0,%1,%2\t # Accumulation of 4 byte dot products Vect/Vect
 pv.sdotsp.sci.b \t%0,%1,%W2\t # Accumulation of 4 byte dot products Vect/Imm"
[(set_attr "type" "arith,arith")
 (set_attr "mode" "SI,SI")]
)

(define_insn "sdotspscv4qi_le"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(plus:SI
		(plus:SI
			(plus:SI
				(mult:SI
					(sign_extend:SI (vec_select:QI (match_operand:V4QI 1 "register_operand" "r") (parallel [(const_int 0)])))
					(sign_extend:SI (subreg:QI (match_operand:SI 2 "register_operand" "r") 0))
				)
				(mult:SI
					(sign_extend:SI (vec_select:QI (match_dup 1) (parallel [(const_int 1)])))
					(sign_extend:SI (subreg:QI (match_dup 2) 0))
				)
			)
			(plus:SI
				(mult:SI
					(sign_extend:SI (vec_select:QI (match_dup 1) (parallel [(const_int 2)])))
					(sign_extend:SI (subreg:QI (match_dup 2) 0))
				)
				(mult:SI
					(sign_extend:SI (vec_select:QI (match_dup 1) (parallel [(const_int 3)])))
					(sign_extend:SI (subreg:QI (match_dup 2) 0))
				)
			)
		)
		(match_operand:SI 3 "register_operand" "0")
	)
   )
  ]
"TARGET_PULP_VECT"
"pv.sdotsp.sc.b \t%0,%1,%2\t # Accumulation of Vect/Scalar reg 4 dot product"
[(set_attr "type" "arith")
 (set_attr "mode" "SI")]
)

(define_insn "udot_prodv4qi"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
	(plus:SI
		(plus:SI
			(plus:SI
				(mult:SI
					(zero_extend:SI (vec_select:QI (match_operand:V4QI 1 "register_operand" "r,r") (parallel [(const_int 0)])))
					(zero_extend:SI (vec_select:QI (match_operand:V4QI 2 "nonmemory_operand" "r,vIusc") (parallel [(const_int 0)])))
				)
				(mult:SI
					(zero_extend:SI (vec_select:QI (match_dup 1) (parallel [(const_int 1)])))
					(zero_extend:SI (vec_select:QI (match_dup 2) (parallel [(const_int 1)])))
				)
			)
			(plus:SI
				(mult:SI
					(zero_extend:SI (vec_select:QI (match_dup 1) (parallel [(const_int 2)])))
					(zero_extend:SI (vec_select:QI (match_dup 2) (parallel [(const_int 2)])))
				)
				(mult:SI
					(zero_extend:SI (vec_select:QI (match_dup 1) (parallel [(const_int 3)])))
					(zero_extend:SI (vec_select:QI (match_dup 2) (parallel [(const_int 3)])))
				)
			)
		)
		(match_operand:SI 3 "register_operand" "0,0")
	)
   )
  ]
"TARGET_PULP_VECT"
"@
 pv.sdotup.b \t%0,%1,%2\t # Accumulation of 4 byte unsigned dot products Vect/Vect
 pv.sdotup.sci.b \t%0,%1,%w2\t # Accumulation of 4 byte unsigned dot products Vect/Imm"
[(set_attr "type" "arith,arith")
 (set_attr "mode" "SI,SI")]
)

(define_insn "sdotupscv4qi_le"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(plus:SI
		(plus:SI
			(plus:SI
				(mult:SI
					(zero_extend:SI (vec_select:QI (match_operand:V4QI 1 "register_operand" "r") (parallel [(const_int 0)])))
					(zero_extend:SI (subreg:QI (match_operand:SI 2 "register_operand" "r") 0))
				)
				(mult:SI
					(zero_extend:SI (vec_select:QI (match_dup 1) (parallel [(const_int 1)])))
					(zero_extend:SI (subreg:QI (match_dup 2) 0))
				)
			)
			(plus:SI
				(mult:SI
					(zero_extend:SI (vec_select:QI (match_dup 1) (parallel [(const_int 2)])))
					(zero_extend:SI (subreg:QI (match_dup 2) 0))
				)
				(mult:SI
					(zero_extend:SI (vec_select:QI (match_dup 1) (parallel [(const_int 3)])))
					(zero_extend:SI (subreg:QI (match_dup 2) 0))
				)
			)
		)
		(match_operand:SI 3 "register_operand" "0")
	)
   )
  ]
"TARGET_PULP_VECT"
"pv.sdotup.sc.b \t%0,%1,%2\t # Accumulation of Vect/Scalar reg 4 unsigned dot product"
[(set_attr "type" "arith")
 (set_attr "mode" "SI")]
)

(define_insn "sdotuspv4qi"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
	(plus:SI
		(plus:SI
			(plus:SI
				(mult:SI
					(zero_extend:SI (vec_select:QI (match_operand:V4QI 1 "register_operand" "r,r") (parallel [(const_int 0)])))
					(sign_extend:SI (vec_select:QI (match_operand:V4QI 2 "nonmemory_operand" "r,vIsdc") (parallel [(const_int 0)])))
				)
				(mult:SI
					(zero_extend:SI (vec_select:QI (match_dup 1) (parallel [(const_int 1)])))
					(sign_extend:SI (vec_select:QI (match_dup 2) (parallel [(const_int 1)])))
				)
			)
			(plus:SI
				(mult:SI
					(zero_extend:SI (vec_select:QI (match_dup 1) (parallel [(const_int 2)])))
					(sign_extend:SI (vec_select:QI (match_dup 2) (parallel [(const_int 2)])))
				)
				(mult:SI
					(zero_extend:SI (vec_select:QI (match_dup 1) (parallel [(const_int 3)])))
					(sign_extend:SI (vec_select:QI (match_dup 2) (parallel [(const_int 3)])))
				)
			)
		)
		(match_operand:SI 3 "register_operand" "0,0")
	)
   )
  ]
"TARGET_PULP_VECT"
"@
 pv.sdotusp.b \t%0,%1,%2\t # Accumulation of 4 byte unsigned/signed dot products Vect/Vect
 pv.sdotusp.sci.b \t%0,%1,%W2\t # Accumulation of 4 byte unsigned/signed dot products Vect/Imm"
[(set_attr "type" "arith")
 (set_attr "mode" "SI")]
)

(define_insn "sdotuspscv4qi_le"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(plus:SI
		(plus:SI
			(plus:SI
				(mult:SI
					(zero_extend:SI (vec_select:QI (match_operand:V4QI 1 "register_operand" "r") (parallel [(const_int 0)])))
					(sign_extend:SI (subreg:QI (match_operand:SI 2 "register_operand" "r") 0))
				)
				(mult:SI
					(zero_extend:SI (vec_select:QI (match_dup 1) (parallel [(const_int 1)])))
					(sign_extend:SI (subreg:QI (match_dup 2) 0))
				)
			)
			(plus:SI
				(mult:SI
					(zero_extend:SI (vec_select:QI (match_dup 1) (parallel [(const_int 2)])))
					(sign_extend:SI (subreg:QI (match_dup 2) 0))
				)
				(mult:SI
					(zero_extend:SI (vec_select:QI (match_dup 1) (parallel [(const_int 3)])))
					(sign_extend:SI (subreg:QI (match_dup 2) 0))
				)
			)
		)
		(match_operand:SI 3 "register_operand" "0")
	)
   )
  ]
"TARGET_PULP_VECT"
"pv.sdotusp.sc.b \t%0,%1,%2\t # Accumulation of Vect/Scalar reg 4 unsigned/signed dot product"
[(set_attr "type" "arith")
 (set_attr "mode" "SI")]
)

;;
;;  ....................
;;
;;	VECTOR COMPARISONS
;;
;;  ....................

;; Vector compare
(define_code_iterator vec_cmp_op      		[eq ne le lt ge gt gtu ltu geu leu])
(define_code_iterator vec_cmp_op_s     		[eq ne le lt ge gt])
(define_code_iterator vec_cmp_op_u     		[gtu ltu geu leu])

(define_code_attr vec_cmp_op_name  		[(eq "eq") (ne "ne") (gt "gt") (lt "lt") (ge "ge") (le "le") (gtu "gtu") (ltu "ltu") (geu "geu") (leu "leu")])
(define_code_attr vec_cmp_scal_imm_pref 	[(eq  "W") (ne  "W") (gt  "W") (lt  "W") (ge  "W") (le  "W") (gtu   "w") (ltu   "w") (geu   "w") (leu   "w")])
(define_code_attr vec_cmp_scal_imm_sign 	[(eq  "s") (ne  "s") (gt  "s") (lt  "s") (ge  "s") (le  "s") (gtu   "u") (ltu   "u") (geu   "u") (leu   "u")])
(define_code_attr vec_cmp_swap_op_name 		[(eq "eq") (ne "ne") (gt "lt") (lt "gt") (ge "le") (le "ge") (gtu "ltu") (ltu "gtu") (geu "leu") (leu "geu")])

;; Straight Vector Comparisons Vect/Vect,  Vect/ScalarReg,  Vect/ScalarImm

(define_insn "cmp<VMODEALL:vec_type>_<vec_cmp_op_name>"
  [(set (match_operand:VMODEALL 0 "register_operand" "=r,r")
	(vec_cmp_op_s:VMODEALL (match_operand:VMODEALL 1 "register_operand" "r,r")
			     (match_operand:VMODEALL 2 "nonmemory_operand" "r,vIsdc")
	)
   )
  ]
  "TARGET_PULP_VECT"
  "@
  pv.cmp<vec_cmp_op_name>.<VMODEALL:vec_size>\t%0,%1,%2 # cmp vect op
  pv.cmp<vec_cmp_op_name>.sci.<VMODEALL:vec_size>\t%0,%1,%<vec_cmp_scal_imm_pref>2 # cmp vect/imm_scalar op"
  [(set_attr "type" "arith,arith")
   (set_attr "mode" "SI,SI")]
)

(define_insn "cmp<VMODEALL:vec_type>_<vec_cmp_op_name>"
  [(set (match_operand:VMODEALL 0 "register_operand" "=r,r")
	(vec_cmp_op_u:VMODEALL (match_operand:VMODEALL 1 "register_operand" "r,r")
			     (match_operand:VMODEALL 2 "nonmemory_operand" "r,vIusc")
	)
   )
  ]
  "TARGET_PULP_VECT"
  "@
  pv.cmp<vec_cmp_op_name>.<VMODEALL:vec_size>\t%0,%1,%2 # cmp vect op
  pv.cmp<vec_cmp_op_name>.sci.<VMODEALL:vec_size>\t%0,%1,%<vec_cmp_scal_imm_pref>2 # cmp vect/imm_scalar op"
  [(set_attr "type" "arith,arith")
   (set_attr "mode" "SI,SI")]
)

(define_insn "cmp<VMODEALL:vec_type>_sc<vec_cmp_op_name>"
  [(set (match_operand:VMODEALL 0 "register_operand" "=r")
	(vec_cmp_op:VMODEALL (match_operand:VMODEALL 1 "register_operand" "r")
			     (vec_duplicate:VMODEALL (match_operand:<vec_scalar_elmt> 2 "register_operand" "r"))
	)
   )
  ]
  "TARGET_PULP_VECT"
  "pv.cmp<vec_cmp_op_name>.sc.<VMODEALL:vec_size>\t%0,%1,%2 # cmp vect/scalar op"
  [(set_attr "type" "arith")
   (set_attr "mode" "SI")]
)

(define_insn "cmp_swap_<VMODEALL:vec_type>_sc<vec_cmp_op_name>"
  [(set (match_operand:VMODEALL 0 "register_operand" "=r")
	(vec_cmp_op:VMODEALL (vec_duplicate:VMODEALL (match_operand:<vec_scalar_elmt> 1 "register_operand" "r"))
			     (match_operand:VMODEALL 2 "register_operand" "r")
	)
   )
  ]
  "TARGET_PULP_VECT"
  "pv.cmp<vec_cmp_swap_op_name>.sc.<VMODEALL:vec_size>\t%0,%2,%1 # cmp (swap) vect/scalar op"
  [(set_attr "type" "arith")
   (set_attr "mode" "SI")]
)

;; vcond expansion into vector comparisons

(define_expand "vcond<mode><mode>"
  [(parallel [
     (set (match_operand:VMODEALL 0 "register_operand" "")
          (if_then_else:VMODEALL
		(match_operator 3 "vec_comparison_operator"
            		[(match_operand:VMODEALL 4 "register_operand" "")
             		 (match_operand:VMODEALL 5 "nonmemory_operand" "")]
		)
		(match_operand:VMODEALL 1 "nonmemory_operand" "")
		(match_operand:VMODEALL 2 "nonmemory_operand" "")
	  )
     )
     (clobber (match_scratch:VMODEALL 6 ""))
    ]
   )
  ]
  "TARGET_PULP_VECT"
  {
	bool scalar_reg;
	bool simple_form = (riscv_replicated_const_vector(operands[1], -1, -1) && riscv_replicated_const_vector(operands[2], 0, 0));
	rtx target;

	/* Simple form: OP1=-1, OP2=0  OP0 = (OP1 & M) | (OP2 & ^M) where M=CmpOP3(OP4, OP5)  ==> Strictly equivalent to CmpOP3 output */
	/* Non simple form, need to fully implement OP0 = (OP1 & M) | (OP2 & ^M) */
	/* Scratch = CmpOP3(OP4, OP5)
	   O0      = Scratch & O1	Get True part
	   Scratch = Scratch ^ Scratch  Toggle mask
	   Scratch = Scratch & Scratch	Get false part
	   O0      = O0 | Scratch	Merge both parts
        */

	if (simple_form) target = operands[0];
	else {
		rtx reg = gen_reg_rtx (<MODE>mode);
		target = operands[6] = reg;
	}

	if (GET_CODE(operands[4]) == VEC_DUPLICATE) {
		rtx tmp = operands[4];
		operands[4] = operands[5]; operands[5] = tmp;
		PUT_CODE(operands[3], swap_condition(GET_CODE(operands[3])));
	}
	scalar_reg = ((GET_CODE(operands[5]) == VEC_DUPLICATE) && !riscv_replicated_const_vector(XEXP(operands[5], 0), 0, 0));
	switch (GET_CODE(operands[3])) {
		case EQ:
			if (scalar_reg) emit_insn (gen_cmp<mode>_sceq(target, operands[4], operands[5]));
			else		emit_insn (gen_cmp<mode>_eq(target, operands[4], operands[5]));
			break;
		case NE:
			if (scalar_reg) emit_insn (gen_cmp<mode>_scne(target, operands[4], operands[5]));
			else		emit_insn (gen_cmp<mode>_ne(target, operands[4], operands[5]));
			break;
		case LT:
			if (scalar_reg) emit_insn (gen_cmp<mode>_sclt(target, operands[4], operands[5]));
			else		emit_insn (gen_cmp<mode>_lt(target, operands[4], operands[5]));
			break;
		case LE:
			if (scalar_reg) emit_insn (gen_cmp<mode>_scle(target, operands[4], operands[5]));
			else		emit_insn (gen_cmp<mode>_le(target, operands[4], operands[5]));
			break;
		case GT:
			if (scalar_reg) emit_insn (gen_cmp<mode>_scgt(target, operands[4], operands[5]));
			else		emit_insn (gen_cmp<mode>_gt(target, operands[4], operands[5]));
			break;
		case GE:
			if (scalar_reg) emit_insn (gen_cmp<mode>_scge(target, operands[4], operands[5]));
			else		emit_insn (gen_cmp<mode>_ge(target, operands[4], operands[5]));
			break;
		default: abort();
	}
	if (!simple_form) {
  		rtx Vect_m1[4] = {constm1_rtx, constm1_rtx, constm1_rtx, constm1_rtx};
		rtx m1_vect = gen_rtx_CONST_VECTOR (<MODE>mode, gen_rtvec_v (GET_MODE_NUNITS(<MODE>mode), Vect_m1));

		emit_insn( gen_and<vec_type>3(operands[0], target, operands[1]));
		emit_insn(gen_exor<vec_type>3(target,      target, m1_vect));
		emit_insn( gen_and<vec_type>3(target,      target, operands[2]));
		emit_insn( gen_ior<vec_type>3(operands[0], target, operands[0]));
	}
	DONE;
  }
)

;; vcond/vcondu

(define_expand "vcondu<mode><mode>"
  [(parallel [
     (set (match_operand:VMODEALL 0 "register_operand" "")
          (if_then_else:VMODEALL
		(match_operator 3 "vec_comparison_operator"
            		[(match_operand:VMODEALL 4 "register_operand" "")
             		 (match_operand:VMODEALL 5 "nonmemory_operand" "")]
		)
		(match_operand:VMODEALL 1 "nonmemory_operand" "")
		(match_operand:VMODEALL 2 "nonmemory_operand" "")
	  )
     )
     (clobber (match_scratch:VMODEALL 6 ""))
    ]
   )
  ]
  "TARGET_PULP_VECT"
  {
	bool scalar_reg;
	bool simple_form = (riscv_replicated_const_vector(operands[1], -1, -1) && riscv_replicated_const_vector(operands[2], 0, 0));
	rtx target;

	/* Simple form: OP1=-1, OP2=0  OP0 = (OP1 & M) | (OP2 & ^M) where M=CmpOP3(OP4, OP5)  ==> Strictly equivalent to CmpOP3 output */
	/* Non simple form, need to fully implement OP0 = (OP1 & M) | (OP2 & ^M) */
	/* Scratch = CmpOP3(OP4, OP5)
	   O0      = Scratch & O1	Get True part
	   Scratch = Scratch ^ Scratch  Toggle mask
	   Scratch = Scratch & Scratch	Get false part
	   O0      = O0 | Scratch	Merge both parts
        */

	if (simple_form) target = operands[0];
	else {
		rtx reg = gen_reg_rtx (<MODE>mode);
		target = operands[6] = reg;
	}

	if (GET_CODE(operands[4]) == VEC_DUPLICATE) {
		rtx tmp = operands[4];
		operands[4] = operands[5]; operands[5] = tmp;
		PUT_CODE(operands[3], swap_condition(GET_CODE(operands[3])));
	}
	scalar_reg = ((GET_CODE(operands[5]) == VEC_DUPLICATE) && !riscv_replicated_const_vector(XEXP(operands[5], 0), 0, 0));
	switch (GET_CODE(operands[3])) {
		case EQ:
			if (scalar_reg) emit_insn (gen_cmp<mode>_sceq(target, operands[4], operands[5]));
			else		emit_insn (gen_cmp<mode>_eq(target, operands[4], operands[5]));
			break;
		case NE:
			if (scalar_reg) emit_insn (gen_cmp<mode>_scne(target, operands[4], operands[5]));
			else		emit_insn (gen_cmp<mode>_ne(target, operands[4], operands[5]));
			break;
		case LT:
		case LTU:
			if (scalar_reg) emit_insn (gen_cmp<mode>_scltu(target, operands[4], operands[5]));
			else		emit_insn (gen_cmp<mode>_ltu(target, operands[4], operands[5]));
			break;
		case LE:
		case LEU:
			if (scalar_reg) emit_insn (gen_cmp<mode>_scleu(target, operands[4], operands[5]));
			else		emit_insn (gen_cmp<mode>_leu(target, operands[4], operands[5]));
			break;
		case GT:
		case GTU:
			if (scalar_reg) emit_insn (gen_cmp<mode>_scgtu(target, operands[4], operands[5]));
			else		emit_insn (gen_cmp<mode>_gtu(target, operands[4], operands[5]));
			break;
		case GE:
		case GEU:
			if (scalar_reg) emit_insn (gen_cmp<mode>_scgeu(target, operands[4], operands[5]));
			else		emit_insn (gen_cmp<mode>_geu(target, operands[4], operands[5]));
			break;
		default: abort();
	}
	if (!simple_form) {
  		rtx Vect_m1[4] = {constm1_rtx, constm1_rtx, constm1_rtx, constm1_rtx};
		rtx m1_vect = gen_rtx_CONST_VECTOR (<MODE>mode, gen_rtvec_v (GET_MODE_NUNITS(<MODE>mode), Vect_m1));

		emit_insn( gen_and<vec_type>3(operands[0], target, operands[1]));
		emit_insn(gen_exor<vec_type>3(target,      target, m1_vect));
		emit_insn( gen_and<vec_type>3(target,      target, operands[2]));
		emit_insn( gen_ior<vec_type>3(operands[0], target, operands[0]));
	}
	DONE;
  }
)

