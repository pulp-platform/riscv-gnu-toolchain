;; PULP only
;; Hardware loops
;;

(define_insn "set_hwloop_lpstart"
 [(set (match_operand:SI 0 "ls_register_operand" "=u")
       (label_ref (match_operand 1 "" ""))
  )
  (use (match_operand:SI 2 "immediate_operand" "I"))
 ]
 "TARGET_PULP_HWLOOP"
 "lp.starti\tx%2,%1\t # loop setup, start set"
 [(set_attr "type" "move")
  (set_attr "mode" "SI")]
)

(define_insn "set_hwloop_lpend"
 [(set (match_operand:SI 0 "le_register_operand" "=t")
       (label_ref (match_operand 1 "" ""))
  )
  (use (match_operand:SI 2 "immediate_operand" "I"))
 ]
 "TARGET_PULP_HWLOOP"
 "lp.endi \tx%2,(%1)\t # loop setup, end set"
 [(set_attr "type" "move")
  (set_attr "mode" "SI")]
)

(define_insn "set_hwloop_lc"
 [(set (match_operand:SI 0 "lc_register_operand" "=k,k")
       (unspec_volatile:SI [(match_operand:SI 1 "general_operand" "r,I")] UNSPECV_LC_SET))
  (use (match_operand:SI 2 "immediate_operand" "I,I"))
 ]
 "TARGET_PULP_HWLOOP"
 "@
  lp.count  \tx%2,%1\t # loop setup, lc set
  lp.counti \tx%2,%1\t # loop setup, lc set"
 [(set_attr "type" "move,move")
  (set_attr "mode" "SI")]
)

(define_insn "set_hwloop_lc_le"
 [(set (match_operand:SI 0 "lc_register_operand" "=k,k")
       (unspec_volatile:SI [(match_operand:SI 1 "general_operand" "r,I")] UNSPECV_LC_SET))
  (set (match_operand:SI 2 "le_register_operand" "=t,t")
       (label_ref (match_operand 3 "" "")))
  (use (match_operand:SI 4 "immediate_operand" "I,I"))
 ]
 "TARGET_PULP_HWLOOP"
 "@
  lp.setup  \tx%4,%1,(%3)\t # loop setup, lc+le set
  lp.setupi \tx%4,%1,(%3)\t # loop setup, lc+le set"
 [(set_attr "type" "move,move")
  (set_attr "mode" "SI")]
)

(define_insn "hw_loop_prolog"
 [(set (match_operand:SI 0 "register_operand" "=r")
       (unspec_volatile: SI [(match_operand:SI 1 "immediate_operand" "I")] UNSPECV_ALLOC))
 ]
 "TARGET_PULP_HWLOOP"
 " # HW Loop prolog"
 [(set_attr "type" "move")
  (set_attr "mode" "SI")]
)

(define_insn "zero_cost_loop_end"
   [(set (pc)
        (if_then_else (ne (match_operand:SI 2 "nonimmediate_operand" "0,0")
                          (const_int 1))
                      (label_ref (match_operand 1 "" ""))
                      (pc)))
   (set (match_operand:SI 0 "nonimmediate_operand" "=r,m")
        (plus (match_dup 2)
              (const_int -1)))
   (unspec [(const_int 0)] UNSPEC_LSETUP_END)
   (clobber (match_scratch:SI 3 "=X,&r"))]
  "TARGET_PULP_HWLOOP && optimize"
  "#"
  [(set_attr "length" "0")]
;;  [(set_attr "type"   "branch")
;;   (set_attr "mode"   "none")]
)

(define_split
  [(set (pc)
        (if_then_else (ne (match_operand:SI 2 "nonimmediate_operand" "")
                          (const_int 1))
                      (label_ref (match_operand 1 "" ""))
                      (pc)))
   (set (match_operand:SI 0 "nonimmediate_operand" "")
        (plus:SI (match_dup 2)
                 (const_int -1)))
   (unspec [(const_int 0)] UNSPEC_LSETUP_END)
   (clobber (match_scratch 3))]
  "TARGET_PULP_HWLOOP && reload_completed"
  [(const_int 0)]
{
  if (!REG_P (operands[0]))
    {
      rtx test;

      /* Fallback into a normal conditional branch insn.  */
      emit_move_insn (operands[3], operands[0]);
      emit_insn (gen_addsi3 (operands[3], operands[3], constm1_rtx));
      emit_move_insn (operands[0], operands[3]);
      test = gen_rtx_NE (VOIDmode, operands[3], const0_rtx);
      emit_jump_insn (gen_cbranchsi4 (test, operands[3],
                                      const0_rtx, operands[1]));
    }
  else
    {
      emit_jump_insn (gen_loop_end (operands[0], operands[1], operands[2]));
    }

  DONE;
})


;; operand 0 is the loop count pseudo register
;; operand 1 is the label to jump to at the top of the loop
(define_expand "doloop_end"
  [(parallel [(set (pc) (if_then_else
                          (ne (match_operand:SI 0 "" "")
                              (const_int 1))
                          (label_ref (match_operand 1 "" ""))
                          (pc)))
              (set (match_dup 0) (plus:SI (match_dup 0) (const_int -1)))
              (unspec [(const_int 0)] UNSPEC_LSETUP_END)
              (clobber (match_dup 2))
            ])]
  "TARGET_PULP_HWLOOP"
{
  /* The loop optimizer doesn't check the predicates... */
  if (GET_MODE (operands[0]) != SImode)
    FAIL;
  riscv_hardware_loop ();
  operands[2]= gen_rtx_SCRATCH(SImode);
})

(define_insn "loop_end"
  [(set (pc)
        (if_then_else (ne (match_operand:SI 2 "register_operand" "0")
                          (const_int 1))
                      (label_ref (match_operand 1 "" ""))
                      (pc)))
   (set (match_operand:SI 0 "register_operand" "=r") (plus (match_dup 2) (const_int -1)))
   (unspec [(const_int 0)] UNSPEC_LSETUP_END)]
 "TARGET_PULP_HWLOOP"
 "/* loop end %0 %l1 */ "
  [(set_attr "length" "0")]
;; [(set_attr "type" "branch")
;;  (set_attr "mode" "none")
;; ]
)

