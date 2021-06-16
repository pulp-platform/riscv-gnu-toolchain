;;
;;  ....................
;;
;;	BIT MANIPULATION
;;
;;  ....................
;;

;; PULP only
(define_insn "popcountsi2"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (popcount:SI (match_operand:SI 1 "register_operand" "r")
        )
   )
  ]
"((Pulp_Cpu>=PULP_V0) && !TARGET_MASK_NOBITOP)"
"p.cnt \t%0,%1\t# count bit set to 1"
[(set_attr "type" "arith")
 (set_attr "mode" "SI")]
)

(define_insn "clrsbsi2"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (clrsb:SI (match_operand:SI 1 "register_operand" "r"))
   )
  ]
"((Pulp_Cpu>=PULP_V0) && !TARGET_MASK_NOBITOP)"
"p.clb \t%0, %1\t # count leading bits, int"
[(set_attr "type" "arith")
 (set_attr "mode" "SI")]
)

(define_insn "fl1si2"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (minus:SI (const_int 31)
                  (clz:SI (match_operand:SI 1 "register_operand" "r"))
        )
   )
  ]
"((Pulp_Cpu>=PULP_V0) && !TARGET_MASK_NOBITOP)"
"p.fl1 \t%0,%1\t# position of first set bit from msb"
[(set_attr "type" "arith")
 (set_attr "mode" "SI")]
)


(define_expand "clzsi2"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (clz:SI (match_operand:SI 1 "register_operand" "r")
        )
   )
  ]
"((Pulp_Cpu>=PULP_V0) && !TARGET_MASK_NOBITOP)"
"
{
        rtx reg = gen_reg_rtx (SImode);

        emit_insn (gen_rtx_SET (reg, gen_rtx_CONST_INT(SImode, 31)));
        emit_insn (gen_fl1si2(operands[0], operands[1]));
        emit_insn (gen_subsi3(operands[0], reg, operands[0]));
        DONE;
}"
)

(define_insn "ctzsi2"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (ctz:SI (match_operand:SI 1 "register_operand" "r")
        )
   )
  ]
"((Pulp_Cpu>=PULP_V0) && !TARGET_MASK_NOBITOP)"
"p.ff1 \t%0,%1\t# position of first set bit from lsb"
[(set_attr "type" "arith")
 (set_attr "mode" "SI")]
)

(define_expand "paritysi2"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (parity:SI (match_operand:SI 1 "register_operand" "r"))
   )
  ]
"((Pulp_Cpu>=PULP_V0) && !TARGET_MASK_NOBITOP)"
"
{
        emit_insn (gen_popcountsi2(operands[0], operands[1]));
        emit_insn (gen_extzvsi(operands[0], operands[0], gen_rtx_CONST_INT(SImode, 1), gen_rtx_CONST_INT(SImode, 0)));
        DONE;
}"
)

;;
;;  ....................
;;
;;	ROTATE RIGHT
;;
;;  ....................
;;

;; PULP only

(define_insn "rotrsi3"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (rotatert:SI (match_operand:SI 1 "register_operand" "r")
                     (match_operand:SI 2 "register_operand" "r")))]
  "((Pulp_Cpu>=PULP_V0) && !TARGET_MASK_NOBITOP)"
  "p.ror \t%0,%1,%2\t# rotate"
[(set_attr "type" "arith")
 (set_attr "mode" "SI")]
)

