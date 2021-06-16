;;
;;  ....................
;;
;;	CLIP/CLIPU
;;
;;  ....................

;; PULP only

(define_insn "clip_maxmin"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (smax:SI (smin:SI (match_operand:SI 1 "register_operand" "r")
                          (match_operand:SI 2 "immediate_operand" "i"))
                 (match_operand:SI 3 "immediate_operand" "i")))]
  "(Pulp_Cpu>=PULP_V2) && !TARGET_MASK_NOCLIP && riscv_valid_clip_operands (operands[2], operands[3], 1)"
  "p.clip\\t%0,%1,%B2"
[(set_attr "type" "logical")
 (set_attr "mode" "SI")]
)

(define_insn "clip_minmax"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (smin:SI (smax:SI (match_operand:SI 1 "register_operand" "r")
                          (match_operand:SI 2 "immediate_operand" "i"))
                 (match_operand:SI 3 "immediate_operand" "i")))]
  "(Pulp_Cpu>=PULP_V2) && !TARGET_MASK_NOCLIP && riscv_valid_clip_operands (operands[3], operands[2], 1)"
  "p.clip\\t%0,%1,%B3"
[(set_attr "type" "logical")
 (set_attr "mode" "SI")]
)

(define_insn"clip_minmax_reg"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (smin:SI (smax:SI (match_operand:SI 1 "register_operand" "r")
			  (neg:SI (plus:SI (match_operand:SI 2 "register_operand" "r") (const_int 1)))
		 )
		 (match_dup 2)))]
  "(Pulp_Cpu>=PULP_V2) && !TARGET_MASK_NOCLIP"
  "p.clipr\\t%0,%1,%2"
[(set_attr "type" "logical")
 (set_attr "mode" "SI")]
)


(define_insn"clip_maxmin_reg"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (smax:SI (smin:SI (match_operand:SI 1 "register_operand" "r")
			  (match_operand:SI 2 "register_operand" "r")
		 )
		 (neg:SI (plus:SI (match_dup 2) (const_int 1)))))]
  "(Pulp_Cpu>=PULP_V2) && !TARGET_MASK_NOCLIP"
  "p.clipr\\t%0,%1,%2"
[(set_attr "type" "logical")
 (set_attr "mode" "SI")]
)

(define_insn "clipu_maxmin"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (smax:SI (smin:SI (match_operand:SI 1 "register_operand" "r")
                          (match_operand:SI 2 "immediate_operand" "i"))
                 (match_operand:SI 3 "immediate_operand" "i")))]
  "(Pulp_Cpu>=PULP_V2) && !TARGET_MASK_NOCLIP && riscv_valid_clip_operands (operands[2], operands[3], 0)"
  "p.clipu\\t%0,%1,%B2"
[(set_attr "type" "logical")
 (set_attr "mode" "SI")]
)

(define_insn "clipu_minmax"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (smin:SI (smax:SI (match_operand:SI 1 "register_operand" "r")
                          (match_operand:SI 2 "immediate_operand" "i"))
                 (match_operand:SI 3 "immediate_operand" "i")))]
  "(Pulp_Cpu>=PULP_V2) && !TARGET_MASK_NOCLIP && riscv_valid_clip_operands (operands[3], operands[2], 0)"
  "p.clipu\\t%0,%1,%B3"
[(set_attr "type" "logical")
 (set_attr "mode" "SI")]
)

(define_insn "clipu_maxmin_reg"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (smax:SI (smin:SI (match_operand:SI 1 "register_operand" "r")
                          (match_operand:SI 2 "register_operand" "r")
		 )
		 (const_int 0)
        )
   )
  ]
  "(Pulp_Cpu>=PULP_V2) && !TARGET_MASK_NOCLIP"
  "p.clipur\\t%0,%1,%2"
[(set_attr "type" "logical")
 (set_attr "mode" "SI")]
)

(define_insn "clipu_minmax_reg"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (smin:SI (smax:SI (match_operand:SI 1 "register_operand" "r") (const_int 0))
		 (match_operand:SI 2 "register_operand" "r")
        )
   )
  ]
  "(Pulp_Cpu>=PULP_V2) && !TARGET_MASK_NOCLIP"
  "p.clipur\\t%0,%1,%2"
[(set_attr "type" "logical")
 (set_attr "mode" "SI")]
)

