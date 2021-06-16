;; Pulp Only
(define_insn "sminsi3"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
        (smin:SI (match_operand:SI 1 "register_operand" "r,r")
                 (match_operand:SI 2 "nonmemory_operand" "r,J")
        )
   )
  ]
"((Pulp_Cpu>=PULP_V0) && !TARGET_MASK_NOMINMAX)"
"@
 p.min \t%0,%1,%2\t# signed min
 p.min \t%0,%1,x0\t# signed min 0"
[(set_attr "type" "arith,arith")
 (set_attr "mode" "SI")]
)

(define_insn "smaxsi3"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
        (smax:SI (match_operand:SI 1 "register_operand" "r,r")
                 (match_operand:SI 2 "nonmemory_operand" "r,J")
        )
   )
  ]
"((Pulp_Cpu>=PULP_V0) && !TARGET_MASK_NOMINMAX)"
"@
 p.max \t%0,%1,%2\t# signed max
 p.max \t%0,%1,x0\t# signed max 0"
[(set_attr "type" "arith,arith")
 (set_attr "mode" "SI")]
)

(define_insn "uminsi3"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (umin:SI (match_operand:SI 1 "register_operand" "r")
                 (match_operand:SI 2 "register_operand" "r")
        )
   )
  ]
"((Pulp_Cpu>=PULP_V0) && !TARGET_MASK_NOMINMAX)"
"p.minu \t%0,%1,%2\t# unsigned min"
[(set_attr "type" "arith")
 (set_attr "mode" "SI")]
)

(define_insn "umaxsi3"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (umax:SI (match_operand:SI 1 "register_operand" "r")
                 (match_operand:SI 2 "register_operand" "r")
        )
   )
  ]
"((Pulp_Cpu>=PULP_V0) && !TARGET_MASK_NOMINMAX)"
"p.maxu \t%0,%1,%2\t# signed max"
[(set_attr "type" "arith")
 (set_attr "mode" "SI")]
)

;;
;;  ....................
;;
;;      AVG, AVGU
;;
;;  ....................

;; PULP only
(define_insn "avgsi3"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (ashiftrt:SI
                (plus:SI (match_operand:SI 1 "register_operand" "r")
                         (match_operand:SI 2 "reg_or_0_operand" "rJ"))
                (const_int 1)
        )
   )
  ]
"((Pulp_Cpu>=PULP_V0) && !TARGET_MASK_NOMINMAX)"
{ return (Pulp_Cpu >= PULP_V2) ? "p.addN \t%0,%1,%z2,1" : "p.avg \t%0,%1,%z2"; }
[(set_attr "type" "arith")
 (set_attr "mode" "SI")]
)

(define_insn "avgusi3"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (lshiftrt:SI
                (plus:SI (match_operand:SI 1 "register_operand" "r")
                         (match_operand:SI 2 "reg_or_0_operand" "rJ"))
                (const_int 1)
        )
   )
  ]
"((Pulp_Cpu>=PULP_V0) && !TARGET_MASK_NOMINMAX)"
{ return (Pulp_Cpu >= PULP_V2) ? "p.adduN \t%0,%1,%z2,1" : "p.avgu \t%0,%1,%z2"; }
[(set_attr "type" "arith")
 (set_attr "mode" "SI")]
)

