(define_insn "load<mode>_ind_reg_reg"
  [(set (match_operand:SUBDISF 0 "register_operand" "=r")
        (mem:SUBDISF (plus:SI (match_operand:SI 1 "register_operand" "r")
                              (match_operand:SI 2 "register_operand" "r")))
   )
  ]
  "((Pulp_Cpu>=PULP_V0) && !TARGET_MASK_NOINDREGREG)"
  "p.l<size_load_store>\t%0,%2(%1)\t# load reg(reg)"
  [(set_attr "type" "load")
   (set_attr "mode" "<LDSTINDMODE>")]
)

(define_insn "load<mode>_<u>ext_ind_reg_reg"
  [(set (match_operand:SUBDISF 0 "register_operand" "=r")
        (mem:SUBDISF (any_extend: SI (plus:SI (match_operand:SI 1 "register_operand" "r")
                                     (match_operand:SI 2 "register_operand" "r"))))
   )
  ]
  "((Pulp_Cpu>=PULP_V0) && !TARGET_MASK_NOINDREGREG)"
  "p.l<size_load_store><u>\t%0,%2(%1)\t# load reg(reg), ext"
  [(set_attr "type" "load")
   (set_attr "mode" "<LDSTINDMODE>")]
)

(define_insn "store<mode>_ind_reg_reg"
  [(set (mem:SUBDISF (plus:SI (match_operand:SI 0 "register_operand" "r,r")
                              (match_operand:SI 1 "register_operand" "r,r")))
        (match_operand:SUBDISF 2 "nonmemory_operand" "r,J")
   )
  ]
  "((Pulp_Cpu>=PULP_V0) && !TARGET_MASK_NOINDREGREG)"
  "@
   p.s<size_load_store>\t%2,%1(%0)\t# store reg(reg)
   p.s<size_load_store>\tx0,%1(%0)\t# store 0 reg(reg)"
  [(set_attr "type" "store,store")
   (set_attr "mode" "<LDSTINDMODE>")]
)
