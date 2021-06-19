;; PULP only

(define_insn "*slet<u>_sisi"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (any_le:SI (match_operand:SI 1 "register_operand" "r")
                   (match_operand:SI 2 "register_operand" "r")))]
  "TARGET_PULP_SLET"
  "p.slet<u>\t%0,%1,%2"
  [(set_attr "type" "slt")
   (set_attr "mode" "SI")])

(define_insn "*sget<u>_sisi"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (any_ge:SI (match_operand:SI 1 "register_operand" "r")
                   (match_operand:SI 2 "register_operand" "r")))]
  "TARGET_PULP_SLET"
  "p.slet<u>\t%0,%2,%1"
  [(set_attr "type" "slt")
   (set_attr "mode" "SI")])
