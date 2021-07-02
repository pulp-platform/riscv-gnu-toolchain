;;
;;  ....................
;;
;;	PARTIAL PRODUCTS (16x16 into 32)
;;
;;  ....................
;;

;; PULP only
;; Standard gcc patterns

(define_insn "<su_mod_alt>mul<SHORT:mode>si3"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (mult:SI (any_extend:SI (match_operand:SHORT 1 "register_operand" "r"))
                 (any_extend:SI (match_operand:SHORT 2 "register_operand" "r")))
   )]
"TARGET_PULP_MULRN_HI"
"p.mul<su_mod> \t%0,%1,%2"
  [(set_attr "type" "imul")
   (set_attr "mode" "SI")]
)

(define_insn "smulhi3_highpart"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (ashiftrt:SI
                (mult:SI (sign_extend:SI (match_operand:HI 1 "register_operand" "r"))
                         (sign_extend:SI (match_operand:HI 2 "register_operand" "r"))
                )
                (const_int 16)
        )
   )
  ]
  "TARGET_PULP_MULRN_HI"
  "p.mulsN \t%0,%1,%2,16\t # mul16x16 into 32 with right shift 16"
  [(set_attr "type" "imul")
   (set_attr "mode" "SI")]
)

(define_insn "umulhi3_highpart"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (lshiftrt:SI
                (mult:SI (zero_extend:SI (match_operand:HI 1 "register_operand" "r"))
                         (zero_extend:SI (match_operand:HI 2 "register_operand" "r"))
                )
                (const_int 16)
        )
   )
  ]
  "TARGET_PULP_MULRN_HI"
  "p.muluN \t%0,%1,%2,16\t # uns mul16x16 into 32 with right logical shift 16"
  [(set_attr "type" "imul")
   (set_attr "mode" "SI")]
)

;; Non standard gcc patterns

(define_insn "mulhhs_si3"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (mult:SI (ashiftrt:SI (match_operand:SI 1 "register_operand" "r") (const_int 16))
                 (ashiftrt:SI (match_operand:SI 2 "register_operand" "r") (const_int 16))
        )
   )
  ]
  "TARGET_PULP_MULRN_HI"
  "p.mulhhs \t%0,%1,%2"
  [(set_attr "type" "imul")
   (set_attr "mode" "SI")]
)

(define_insn "mulhhu_si3"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (mult:SI (lshiftrt:SI (match_operand:SI 1 "register_operand" "r") (const_int 16))
                 (lshiftrt:SI (match_operand:SI 2 "register_operand" "r") (const_int 16))
        )
   )
  ]
  "TARGET_PULP_MULRN_HI"
  "p.mulhhu \t%0,%1,%2"
  [(set_attr "type" "imul")
   (set_attr "mode" "SI")]
)

;;
;;  ....................
;;
;;	PARTIAL MULT (16x16 into 32) WITH NORM AND ROUNDING
;;
;;  ....................
;;

;; PULP only

(define_insn "mulsNr_si3"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (ashiftrt:SI
                (mult:SI (sign_extend:SI (match_operand:HI 1 "register_operand" "r"))
                         (sign_extend:SI (match_operand:HI 2 "register_operand" "r"))
                )
                (match_operand:SI 3 "immediate_operand" "i")
        )
   )
  ]
  "TARGET_PULP_MULRN_HI && riscv_valid_norm_round_imm_op(operands[3], NULL, 31)"
  "p.mulsN \t%0,%1,%2,%3"
  [(set_attr "type" "imul")
   (set_attr "mode" "SI")]
)

(define_insn "mulsRNr_si3"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (ashiftrt:SI
                (plus:SI
                        (mult:SI (sign_extend:SI (match_operand:HI 1 "register_operand" "r"))
                                 (sign_extend:SI (match_operand:HI 2 "register_operand" "r"))
                        )
                        (match_operand:SI 4 "immediate_operand" "i")
                )
                (match_operand:SI 3 "immediate_operand" "i")
        )
   )
  ]
  "TARGET_PULP_MULRN_HI && riscv_valid_norm_round_imm_op(operands[3], operands[4], 31)"
  "p.mulsRN \t%0,%1,%2,%3"
  [(set_attr "type" "imul")
   (set_attr "mode" "SI")]
)


(define_insn "mulsRNr_hi3"
  [(set (match_operand:HI 0 "register_operand" "=r")
	(truncate:HI
        	(ashiftrt:SI
                	(plus:SI
                        	(mult:SI (sign_extend:SI (match_operand:HI 1 "register_operand" "r"))
                                 	 (sign_extend:SI (match_operand:HI 2 "register_operand" "r"))
                        	)
                        	(match_operand:SI 4 "immediate_operand" "i")
                	)
                	(match_operand:SI 3 "immediate_operand" "i")
        	)
	)
   )
  ]
  "TARGET_PULP_MULRN_HI && riscv_valid_norm_round_imm_op(operands[3], operands[4], 15)"
  "p.mulsRN \t%0,%1,%2,%3"
  [(set_attr "type" "imul")
   (set_attr "mode" "SI")]
)




(define_insn "muluNr_si3"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (lshiftrt:SI
                (mult:SI (zero_extend:SI (match_operand:HI 1 "register_operand" "r"))
                         (zero_extend:SI (match_operand:HI 2 "register_operand" "r"))
                )
                (match_operand:SI 3 "immediate_operand" "i")
        )
   )
  ]
  "TARGET_PULP_MULRN_HI && riscv_valid_norm_round_imm_op(operands[3], NULL, 31)"
  "p.muluN \t%0,%1,%2,%3"
  [(set_attr "type" "imul")
   (set_attr "mode" "SI")]
)

(define_insn "muluRNr_si3"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (lshiftrt:SI
                (plus:SI
                        (mult:SI (zero_extend:SI (match_operand:HI 1 "register_operand" "r"))
                                 (zero_extend:SI (match_operand:HI 2 "register_operand" "r"))
                        )
                        (match_operand:SI 4 "immediate_operand" "i")
                )
                (match_operand:SI 3 "immediate_operand" "i")
        )
   )
  ]
  "TARGET_PULP_MULRN_HI && riscv_valid_norm_round_imm_op(operands[3], operands[4], 31)"
  "p.muluRN \t%0,%1,%2,%3"
  [(set_attr "type" "imul")
   (set_attr "mode" "SI")]
)

(define_insn "mulhhsNr_si3"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (ashiftrt:SI
                (mult:SI (ashiftrt:SI (match_operand:SI 1 "register_operand" "r") (const_int 16))
                         (ashiftrt:SI (match_operand:SI 2 "register_operand" "r") (const_int 16))
                )
                (match_operand:SI 3 "immediate_operand" "i")
        )
   )
  ]
  "TARGET_PULP_MULRN_HI && riscv_valid_norm_round_imm_op(operands[3], NULL, 31)"
  "p.mulhhsN \t%0,%1,%2,%3"
  [(set_attr "type" "imul")
   (set_attr "mode" "SI")]
)

(define_insn "mulhhuNr_si3"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (lshiftrt:SI
                (mult:SI (lshiftrt:SI (match_operand:SI 1 "register_operand" "r") (const_int 16))
                         (lshiftrt:SI (match_operand:SI 2 "register_operand" "r") (const_int 16))
                )
                (match_operand:SI 3 "immediate_operand" "i")
        )
   )
  ]
  "TARGET_PULP_MULRN_HI && riscv_valid_norm_round_imm_op(operands[3], NULL, 31)"
  "p.mulhhuN \t%0,%1,%2,%3"
  [(set_attr "type" "imul")
   (set_attr "mode" "SI")]
)

(define_insn "mulhhsRNr_si3"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (ashiftrt:SI
                (plus:SI
                        (mult:SI (ashiftrt:SI (match_operand:SI 1 "register_operand" "r") (const_int 16))
                                 (ashiftrt:SI (match_operand:SI 2 "register_operand" "r") (const_int 16))
                        )
                        (match_operand:SI 4 "immediate_operand" "i")
                )
                (match_operand:SI 3 "immediate_operand" "i")
        )
   )
  ]
  "TARGET_PULP_MULRN_HI && riscv_valid_norm_round_imm_op(operands[3], operands[4], 31)"
  "p.mulhhsRN \t%0,%1,%2,%3"
  [(set_attr "type" "imul")
   (set_attr "mode" "SI")]
)

(define_insn "mulhhuRNr_si3"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (lshiftrt:SI
                (plus:SI
                        (mult:SI (lshiftrt:SI (match_operand:SI 1 "register_operand" "r") (const_int 16))
                                 (lshiftrt:SI (match_operand:SI 2 "register_operand" "r") (const_int 16))
                        )
                        (match_operand:SI 4 "immediate_operand" "i")
                )
                (match_operand:SI 3 "immediate_operand" "i")
        )
   )
  ]
  "TARGET_PULP_MULRN_HI && riscv_valid_norm_round_imm_op(operands[3], operands[4], 31)"
  "p.mulhhuRN \t%0,%1,%2,%3"
  [(set_attr "type" "imul")
   (set_attr "mode" "SI")]
)

;;
;;  ....................
;;
;;	PARTIAL MAC (16x16 into 32)
;;
;;  ....................

;; PULP only

(define_insn "macs<mode>_si4"
  [(set (match_operand:SI 0 "register_operand" "=a,r")
        (plus:SI (mult:SI (sign_extend:SI (match_operand:SHORT 1 "register_operand" "r,r"))
                          (sign_extend:SI (match_operand:SHORT 2 "register_operand" "r,r"))
                 )
                 (match_operand:SI 3 "register_operand" "r,0")
        )
   )
  ]
  "TARGET_PULP_PARTMAC || TARGET_PULP_MAC_ALT"
  "@
   p.macs \t%0,%1,%2,%3
   p.macs \t%0,%1,%2"
  [(set_attr "type" "imul,imul")
   (set_attr "mode" "SI")]
)

(define_insn "macu<mode>_si4"
  [(set (match_operand:SI 0 "register_operand" "=a,r")
        (plus:SI (mult:SI (zero_extend:SI (match_operand:SHORT 1 "register_operand" "r,r"))
                          (zero_extend:SI (match_operand:SHORT 2 "register_operand" "r,r"))
                 )
                 (match_operand:SI 3 "register_operand" "r,0")
        )
   )
  ]
  "TARGET_PULP_PARTMAC || TARGET_PULP_MAC_ALT"
  "@
   p.macu \t%0,%1,%2,%3
   p.macu \t%0,%1,%2"
  [(set_attr "type" "imul,imul")
   (set_attr "mode" "SI")]
)

(define_insn "machlsu_si4"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (plus:SI (mult:SI (ashiftrt:SI (match_operand:SI 1 "register_operand" "r") (const_int 16))
                          (zero_extend:SI (match_operand:HI 2 "register_operand" "r"))
                 )
                 (match_operand:SI 3 "register_operand" "r")
        )
   )
  ]
  "TARGET_PULP_MAC_ALT"
  "p.machlsu \t%0,%1,%2,%3"
  [(set_attr "type" "imul")
   (set_attr "mode" "SI")]
)

(define_insn "machlu_si4"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (plus:SI (mult:SI (lshiftrt:SI (match_operand:SI 1 "register_operand" "r") (const_int 16))
                          (zero_extend:SI (match_operand:HI 2 "register_operand" "r"))
                 )
                 (match_operand:SI 3 "register_operand" "r")
        )
   )
  ]
  "TARGET_PULP_MAC_ALT"
  "p.machlu \t%0,%1,%2,%3"
  [(set_attr "type" "imul")
   (set_attr "mode" "SI")]
)

(define_insn "machhs_si4"
  [(set (match_operand:SI 0 "register_operand" "=a,r")
        (plus:SI (mult:SI (ashiftrt:SI (match_operand:SI 1 "register_operand" "r,r") (const_int 16))
                          (ashiftrt:SI (match_operand:SI 2 "register_operand" "r,r") (const_int 16))
                 )
                 (match_operand:SI 3 "register_operand" "r,0")
        )
   )
  ]
  "TARGET_PULP_PARTMAC || TARGET_PULP_MAC_ALT"
  "@
   p.machhs \t%0,%1,%2,%3
   p.machhs \t%0,%1,%2"
  [(set_attr "type" "imul,imul")
   (set_attr "length" "1")]
)

(define_insn "machhu_si4"
  [(set (match_operand:SI 0 "register_operand" "=a,r")
        (plus:SI (mult:SI (lshiftrt:SI (match_operand:SI 1 "register_operand" "r,r") (const_int 16))
                          (lshiftrt:SI (match_operand:SI 2 "register_operand" "r,r") (const_int 16))
                 )
                 (match_operand:SI 3 "register_operand" "r,0")
        )
   )
  ]
  "TARGET_PULP_PARTMAC || TARGET_PULP_MAC_ALT"
  "@
   p.machhu \t%0,%1,%2,%3
   p.machhu \t%0,%1,%2"
  [(set_attr "type" "imul,imul")
   (set_attr "mode" "SI")]
)

(define_insn "machls_si4"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (plus:SI (mult:SI (ashiftrt:SI (match_operand:SI 1 "register_operand" "r") (const_int 16))
                          (sign_extend:SI (match_operand:HI 2 "register_operand" "r"))
                 )
                 (match_operand:SI 3 "register_operand" "r")
        )
   )
  ]
  "TARGET_PULP_MAC_ALT"
  "p.machls \t%0,%1,%2,%3"
  [(set_attr "type" "imul")
   (set_attr "mode" "SI")]
)


;;
;;  ....................
;;
;;	PARTIAL MAC (16x16 into 32) WITH ROUNDING AND NORM
;;
;;  ....................

;; PULP only

(define_insn "macsNr_si3"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (ashiftrt:SI
                (plus:SI
                        (mult:SI (sign_extend:SI (match_operand:HI 1 "register_operand" "r"))
                                 (sign_extend:SI (match_operand:HI 2 "register_operand" "r"))
                        )
                        (match_operand:SI 3 "register_operand" "0")
                )
                (match_operand:SI 4 "immediate_operand" "i")
        )
   )
  ]
  "TARGET_PULP_MACRN_HI && riscv_valid_norm_round_imm_op(operands[4], NULL, 31)"
  "p.macsN \t%0,%1,%2,%4"
  [(set_attr "type" "imul")
   (set_attr "mode" "SI")]
)

(define_insn "macuNr_si3"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (lshiftrt:SI
                (plus:SI
                        (mult:SI (zero_extend:SI (match_operand:HI 1 "register_operand" "r"))
                                 (zero_extend:SI (match_operand:HI 2 "register_operand" "r"))
                        )
                        (match_operand:SI 3 "register_operand" "0")
                )
                (match_operand:SI 4 "immediate_operand" "i")
        )
   )
  ]
  "TARGET_PULP_MACRN_HI && riscv_valid_norm_round_imm_op(operands[4], NULL, 31)"
  "p.macuN \t%0,%1,%2,%4"
  [(set_attr "type" "imul")
   (set_attr "mode" "SI")]
)

(define_insn "macsRNr_si3"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (ashiftrt:SI
                (plus:SI
                        (plus:SI
                                (mult:SI (sign_extend:SI (match_operand:HI 1 "register_operand" "r"))
                                         (sign_extend:SI (match_operand:HI 2 "register_operand" "r"))
                                )
                                (match_operand:SI 3 "register_operand" "0")
                        )
                        (match_operand:SI 5 "immediate_operand" "i")
                )
                (match_operand:SI 4 "immediate_operand" "i")
        )
   )
  ]
  "TARGET_PULP_MACRN_HI && riscv_valid_norm_round_imm_op(operands[4], operands[5], 31)"
  "p.macsRN \t%0,%1,%2,%4"
  [(set_attr "type" "imul")
   (set_attr "mode" "SI")]
)

(define_insn "macuRNr_si3"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (lshiftrt:SI
                (plus:SI
                        (plus:SI
                                (mult:SI (zero_extend:SI (match_operand:HI 1 "register_operand" "r"))
                                         (zero_extend:SI (match_operand:HI 2 "register_operand" "r"))
                                )
                                (match_operand:SI 3 "register_operand" "0")
                        )
                        (match_operand:SI 5 "immediate_operand" "i")
                )
                (match_operand:SI 4 "immediate_operand" "i")
        )
   )
  ]
  "TARGET_PULP_MACRN_HI && riscv_valid_norm_round_imm_op(operands[4], operands[5], 31)"
  "p.macuRN \t%0,%1,%2,%4"
  [(set_attr "type" "imul")
   (set_attr "mode" "SI")]
)

(define_insn "machhsNr_si3"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (ashiftrt:SI
                (plus:SI
                        (mult:SI (ashiftrt:SI (match_operand:SI 1 "register_operand" "r") (const_int 16))
                                 (ashiftrt:SI (match_operand:SI 2 "register_operand" "r") (const_int 16))
                        )
                        (match_operand:SI 3 "register_operand" "0")
                )
                (match_operand:SI 4 "immediate_operand" "i")
        )
   )
  ]
  "TARGET_PULP_MACRN_HI && riscv_valid_norm_round_imm_op(operands[4], NULL, 31)"
  "p.machhsN \t%0,%1,%2,%4"
  [(set_attr "type" "imul")
   (set_attr "mode" "SI")]
)

(define_insn "machhuNr_si3"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (lshiftrt:SI
                (plus:SI
                        (mult:SI (lshiftrt:SI (match_operand:SI 1 "register_operand" "r") (const_int 16))
                                 (lshiftrt:SI (match_operand:SI 2 "register_operand" "r") (const_int 16))
                        )
                        (match_operand:SI 3 "register_operand" "0")
                )
                (match_operand:SI 4 "immediate_operand" "i")
        )
   )
  ]
  "TARGET_PULP_MACRN_HI && riscv_valid_norm_round_imm_op(operands[4], NULL, 31)"
  "p.machhuN \t%0,%1,%2,%4"
  [(set_attr "type" "imul")
   (set_attr "mode" "SI")]
)

(define_insn "machhsRNr_si3"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (ashiftrt:SI
                (plus:SI
                        (plus:SI
                                (mult:SI (ashiftrt:SI (match_operand:SI 1 "register_operand" "r") (const_int 16))
                                         (ashiftrt:SI (match_operand:SI 2 "register_operand" "r") (const_int 16))
                                )
                                (match_operand:SI 3 "register_operand" "0")
                        )
                        (match_operand:SI 5 "immediate_operand" "i")
                )
                (match_operand:SI 4 "immediate_operand" "i")
        )
   )
  ]
  "TARGET_PULP_MACRN_HI && riscv_valid_norm_round_imm_op(operands[4], operands[5], 31)"
  "p.machhsRN \t%0,%1,%2,%4"
  [(set_attr "type" "imul")
   (set_attr "mode" "SI")]
)

(define_insn "machhuRNr_si3"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (lshiftrt:SI
                (plus:SI
                        (plus:SI
                                (mult:SI (lshiftrt:SI (match_operand:SI 1 "register_operand" "r") (const_int 16))
                                         (lshiftrt:SI (match_operand:SI 2 "register_operand" "r") (const_int 16))
                                )
                                (match_operand:SI 3 "register_operand" "0")
                        )
                        (match_operand:SI 5 "immediate_operand" "i")
                )
                (match_operand:SI 4 "immediate_operand" "i")
        )
   )
  ]
  "TARGET_PULP_MACRN_HI && riscv_valid_norm_round_imm_op(operands[4], operands[5], 31)"
  "p.machhuRN \t%0,%1,%2,%4"
  [(set_attr "type" "imul")
   (set_attr "mode" "SI")]
)

;;
;;  ....................
;;
;;	MAC (32x32 into 32)
;;
;;  ....................

;; PULP only

(define_insn "maddsisi4"
  [(set (match_operand:SI 0 "register_operand" "=a,r")
        (plus:SI (mult:SI (match_operand:SI 1 "register_operand" "r,r")
                          (match_operand:SI 2 "register_operand" "r,r"))
                 (match_operand:SI 3 "register_operand" "r,0"))
   )
  ]
"TARGET_PULP_MAC_SI"
"@
 p.mac \t%0,%1,%2,%3\t# mac 32x32 in 32 instruction
 p.mac \t%0,%1,%2\t# mac 32x32 in 32 instruction"
[(set_attr "type" "imul,imul")
 (set_attr "mode" "SI")]
)

(define_insn "msubsisi4"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (minus:SI (match_operand:SI 3 "register_operand" "0")
		  (mult:SI (match_operand:SI 1 "register_operand" "r")
                           (match_operand:SI 2 "register_operand" "r"))
	)
   )
  ]
"TARGET_PULP_MAC_SI"
"p.msu \t%0,%1,%2\t# mac 32x32 in 32 instruction"
[(set_attr "type" "imul")
 (set_attr "mode" "SI")]
)

