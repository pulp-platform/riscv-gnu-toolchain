;; Post modified load and store

(define_insn "load<mode>_ind_postinc"
  [(set (match_operand:SUBDISF 0 "register_operand" "=r")
        (mem:SUBDISF (post_inc:SI (match_operand:SI 1 "register_operand" "+r")))
   )
  ]
  "((Pulp_Cpu>=PULP_V0) && !TARGET_MASK_NOPOSTMOD)"
  "p.l<size_load_store>\t%0,<size_mem>(%1!)\t# load post inc"
  [(set_attr "type" "load")
   (set_attr "mode" "<LDSTINDMODE>")]
)

(define_insn "load<mode>_<u>ext_ind_postinc"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (any_extend:SI
             (mem:SUBDISF (post_inc:SI (match_operand:SI 1 "register_operand" "+r")))
        )
   )
  ]
  "((Pulp_Cpu>=PULP_V0) && !TARGET_MASK_NOPOSTMOD)"
  "p.l<size_load_store><u>\t%0,<size_mem>(%1!)\t# load post inc, ext"
  [(set_attr "type" "load")
   (set_attr "mode" "<LDSTINDMODE>")]
)

(define_insn "load<mode>_ind_postdec"
  [(set (match_operand:SUBDISF 0 "register_operand" "=r")
        (mem:SUBDISF (post_dec:SI (match_operand:SI 1 "register_operand" "+r")))
   )
  ]
  "((Pulp_Cpu>=PULP_V0) && !TARGET_MASK_NOPOSTMOD)"
  "p.l<size_load_store>\t%0,-<size_mem>(%1!)\t# load post dec"
  [(set_attr "type" "load")
   (set_attr "mode" "<LDSTINDMODE>")]
)

(define_insn "load<mode>_<u>ext_ind_postdec"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (any_extend:SI
             (mem:SUBDISF (post_dec:SI (match_operand:SI 1 "register_operand" "+r")))
        )
   )
  ]
  "((Pulp_Cpu>=PULP_V0) && !TARGET_MASK_NOPOSTMOD)"
  "p.l<size_load_store><u>\t%0,-<size_mem>(%1!)\t# load post dec, ext"
  [(set_attr "type" "load")
   (set_attr "mode" "<LDSTINDMODE>")]
)


(define_insn "load<mode>_ind_post_mod"
  [(set (match_operand:SUBDISF 0 "register_operand" "=r,r")
        (mem:SUBDISF (post_modify:SI (match_operand:SI 1 "register_operand" "+r,r")
                                     (plus:SI (match_dup 1) (match_operand:SI 2 "nonmemory_operand" "r,I"))))
   )
  ]
  "((Pulp_Cpu>=PULP_V0) && !TARGET_MASK_NOPOSTMOD)"
  "@
   p.l<size_load_store>\t%0,%2(%1!)\t# load post modify reg
   p.l<size_load_store>\t%0,%2(%1!)\t# load post modify imm"
  [(set_attr "type" "load,load")
   (set_attr "mode" "<LDSTINDMODE>")]
)

(define_insn "load<mode>_<u>ext_ind_post_mod"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
        (any_extend:SI
                    (mem:SUBDISF (post_modify:SI (match_operand:SI 1 "register_operand" "+r,r")
                                                 (plus:SI (match_dup 1) (match_operand:SI 2 "nonmemory_operand" "r,I"))))
        )
   )
  ]
  "((Pulp_Cpu>=PULP_V0) && !TARGET_MASK_NOPOSTMOD)"
  "@
   p.l<size_load_store><u>\t%0,%2(%1!)\t# load post modify reg, ext
   p.l<size_load_store><u>\t%0,%2(%1!)\t# load post modify imm, ext"
  [(set_attr "type" "load,load")
   (set_attr "mode" "<LDSTINDMODE>")]
)

(define_insn "store<mode>_ind_postinc"
  [(set (mem:SUBDISF (post_inc:SI (match_operand:SI 0 "register_operand" "+r,r")))
        (match_operand:SUBDISF 1 "nonmemory_operand" "r,J")
   )
  ]
  "((Pulp_Cpu>=PULP_V0) && !TARGET_MASK_NOPOSTMOD)"
  "@
   p.s<size_load_store>\t%1,<size_mem>(%0!)\t# store post inc
   p.s<size_load_store>\tx0,<size_mem>(%0!)\t# store 0 post inc"
  [(set_attr "type" "store,store")
   (set_attr "mode" "<LDSTINDMODE>")]
)

(define_insn "store<mode>_ind_postdec"
  [(set (mem:SUBDISF (post_dec:SI (match_operand:SI 0 "register_operand" "+r,r")))
        (match_operand:SUBDISF 1 "nonmemory_operand" "r,J")
   )
  ]
  "((Pulp_Cpu>=PULP_V0) && !TARGET_MASK_NOPOSTMOD)"
  "@
   p.s<size_load_store>\t%1,-<size_mem>(%0!)\t# store post dec
   p.s<size_load_store>\tx0,-<size_mem>(%0!)\t# store 0 post dec"
  [(set_attr "type" "store,store")
   (set_attr "mode" "<LDSTINDMODE>")]
)

(define_insn "store<mode>_ind_postmod"
  [(set (mem:SUBDISF (post_modify:SI (match_operand:SI 0 "register_operand" "+r,r,r,r")
                                     (plus:SI (match_dup 0) (match_operand:SI 2 "nonmemory_operand" "r,r,I,I"))))
        (match_operand:SUBDISF 1 "nonmemory_operand" "r,J,r,J")
   )
  ]
  "((Pulp_Cpu>=PULP_V0) && !TARGET_MASK_NOPOSTMOD)"
  "@
   p.s<size_load_store>\t%1,%2(%0!)\t# store post modify reg
   p.s<size_load_store>\tx0,%2(%0!)\t# store 0 post modify reg
   p.s<size_load_store>\t%1,%2(%0!)\t# store post modify imm
   p.s<size_load_store>\tx0,%2(%0!)\t# store 0 post modify imm"
  [(set_attr "type" "store,store,store,store")
   (set_attr "mode" "<LDSTINDMODE>")]
)
