;;
;;  ....................
;;
;;	ADD/SUB WITH ROUNDING AND NORM
;;
;;  ....................

;; PULP only

(define_insn "addN<norm_sign>_si3"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (norm_op:SI
                (plus:SI
                        (match_operand:SI 1 "register_operand" "r")
                        (match_operand:SI 2 "reg_or_0_operand" "rJ")
                )
                (match_operand:SI 3 "immediate_operand" "i")
        )
   )
  ]
  "TARGET_PULP_ADDSUBRN && riscv_valid_norm_round_imm_op(operands[3], NULL, 31)"
  "p.add<norm_sign>N \t%0,%1,%z2,%3"
[(set_attr "type" "arith")
 (set_attr "mode" "SI")]
)

(define_insn "addN<norm_sign>_reg_si3"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (norm_op:SI
                (plus:SI
                        (match_operand:SI 1 "register_operand" "0")
                        (match_operand:SI 2 "reg_or_0_operand" "rJ")
                )
                (match_operand:SI 3 "register_operand" "r")
        )
   )
  ]
  "TARGET_PULP_ADDSUBRN"
  "p.add<norm_sign>Nr \t%0,%z2,%3"
[(set_attr "type" "arith")
 (set_attr "mode" "SI")]
)

(define_insn "subN<norm_sign>_si3"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (norm_op:SI
                (minus:SI
                        (match_operand:SI 1 "register_operand" "r")
                        (match_operand:SI 2 "reg_or_0_operand" "rJ")
                )
                (match_operand:SI 3 "immediate_operand" "i")
        )
   )
  ]
  "TARGET_PULP_ADDSUBRN && riscv_valid_norm_round_imm_op(operands[3], NULL, 31)"
  "p.sub<norm_sign>N \t%0,%1,%z2,%3"
[(set_attr "type" "arith")
 (set_attr "mode" "SI")]
)

(define_insn "subN<norm_sign>_reg_si3"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (norm_op:SI
                (minus:SI
                        (match_operand:SI 1 "register_operand" "0")
                        (match_operand:SI 2 "reg_or_0_operand" "rJ")
                )
                (match_operand:SI 3 "register_operand" "r")
        )
   )
  ]
  "TARGET_PULP_ADDSUBRN"
  "p.sub<norm_sign>Nr \t%0,%z2,%3"
[(set_attr "type" "arith")
 (set_attr "mode" "SI")]
)

(define_insn "addRN<norm_sign>_si3"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (norm_op:SI
                (plus:SI
                        (plus:SI
                                (match_operand:SI 1 "register_operand" "r")
                                (match_operand:SI 2 "reg_or_0_operand" "rJ")
                        )
                        (match_operand:SI 4 "immediate_operand" "i")
                )
                (match_operand:SI 3 "immediate_operand" "i")
        )
   )
  ]
  "TARGET_PULP_ADDSUBRN && riscv_valid_norm_round_imm_op(operands[3], operands[4], 31)"
  "p.add<norm_sign>RN \t%0,%1,%z2,%3"
[(set_attr "type" "arith")
 (set_attr "mode" "SI")]
)


(define_insn "addRN<norm_sign>_reg_si3"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (norm_op:SI
                (plus:SI
                        (plus:SI (match_operand:SI 1 "register_operand" "0")
                                 (match_operand:SI 2 "reg_or_0_operand" "rJ")
                        )
			(ashift:SI (const_int 1)
				   (minus:SI (match_operand:SI 3 "register_operand" "r") (const_int 1))
				   ; (minus:SI (match_operand:SI 3 "nonmemory_operand" "r,i") (const_int 1))
			)
                )
                (match_dup 3)
        )
   )
  ]
  "TARGET_PULP_ADDSUBRN"
  "p.add<norm_sign>RNr \t%0,%z2,%3"
[(set_attr "type" "arith")
 (set_attr "mode" "SI")]
)

(define_insn "subRN<norm_sign>_si3"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (norm_op:SI
                (plus:SI
                        (minus:SI
                                (match_operand:SI 1 "register_operand" "r")
                                (match_operand:SI 2 "reg_or_0_operand" "rJ")
                        )
                        (match_operand:SI 4 "immediate_operand" "i")
                )
                (match_operand:SI 3 "immediate_operand" "i")
        )
   )
  ]
  "TARGET_PULP_ADDSUBRN && riscv_valid_norm_round_imm_op(operands[3], operands[4], 31)"
  "p.sub<norm_sign>RN \t%0,%1,%z2,%3"
[(set_attr "type" "arith")
 (set_attr "mode" "SI")]
)

(define_insn "subRN<norm_sign>_reg_si3"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (norm_op:SI
                (plus:SI
                        (minus:SI
                                (match_operand:SI 1 "register_operand" "0")
                                (match_operand:SI 2 "reg_or_0_operand" "rJ")
                        )
			(ashift:SI (const_int 1)
				   (minus:SI (match_operand:SI 3 "register_operand" "r") (const_int 1))
			)
                )
                (match_dup 3)
        )
   )
  ]
  "TARGET_PULP_ADDSUBRN"
  "p.sub<norm_sign>RNr \t%0,%z2,%3"
[(set_attr "type" "arith")
 (set_attr "mode" "SI")]
)

