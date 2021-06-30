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
"TARGET_PULP_BITOP || TARGET_PULP_BITOP_SMALL"
"p.cnt \t%0,%1\t# count bit set to 1"
[(set_attr "type" "arith")
 (set_attr "mode" "SI")]
)

;; The clrsbsi2 pattern nearly matches the p.clb instruction except for when the
;; argument is zero. In that case clrsbsi2 returns 31 while p.clb returns 0. We
;; work around that. By default, clrsb would use clz but that code would have
;; more special casing.
(define_expand "clrsbsi2"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (clrsb:SI (match_operand:SI 1 "register_operand" "r")))]
  "TARGET_PULP_BITOP || TARGET_PULP_BITOP_SMALL"
{
  rtx dest = operands[0];
  rtx src = operands[1];
  rtx end_label, cmp;
  end_label = gen_label_rtx ();
  emit_move_insn (dest, GEN_INT (31));
  cmp = gen_rtx_EQ (VOIDmode, src, const0_rtx);
  emit_jump_insn (gen_rtx_SET (pc_rtx,
			       gen_rtx_IF_THEN_ELSE (VOIDmode,
				 cmp,
				 gen_rtx_LABEL_REF (VOIDmode, end_label),
				 pc_rtx)));
  emit_insn (gen_pclbsi2 (dest, src));
  emit_label (end_label);
  DONE;
}
  [(set_attr "type" "arith")
   (set_attr "mode" "SI")])

(define_insn "pclbsi2"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (unspec:SI [(match_operand:SI 1 "register_operand" "r")] UNSPEC_PULP_CLB))]
  "TARGET_PULP_BITOP || TARGET_PULP_BITOP_SMALL"
  "p.clb\t%0,%1"
  [(set_attr "type" "arith")
   (set_attr "mode" "SI")])

(define_insn "fl1si2"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (minus:SI (const_int 31)
                  (clz:SI (match_operand:SI 1 "register_operand" "r"))
        )
   )
  ]
"TARGET_PULP_BITOP || TARGET_PULP_BITOP_SMALL"
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
"TARGET_PULP_BITOP || TARGET_PULP_BITOP_SMALL"
"
{
        rtx reg = gen_reg_rtx (SImode);

        emit_insn (gen_rtx_SET (reg, gen_rtx_CONST_INT(SImode, 31)));
        emit_insn (gen_fl1si2(operands[0], operands[1]));
        emit_insn (gen_subsi3(operands[0], reg, operands[0]));
        DONE;
}"
)

;; Note that ctz is used in libgcc to derive ffs
(define_insn "ctzsi2"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (ctz:SI (match_operand:SI 1 "register_operand" "r")
        )
   )
  ]
"TARGET_PULP_BITOP || TARGET_PULP_BITOP_SMALL"
"p.ff1 \t%0,%1\t# position of first set bit from lsb"
[(set_attr "type" "arith")
 (set_attr "mode" "SI")]
)

(define_expand "paritysi2"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (parity:SI (match_operand:SI 1 "register_operand" "r"))
   )
  ]
"TARGET_PULP_BITOP || TARGET_PULP_BITOP_SMALL"
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
  "TARGET_PULP_BITOP || TARGET_PULP_BITOP_SMALL"
  "p.ror \t%0,%1,%2\t# rotate"
[(set_attr "type" "arith")
 (set_attr "mode" "SI")]
)

