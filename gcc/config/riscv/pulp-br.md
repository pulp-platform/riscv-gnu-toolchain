;; PULP branches
(define_insn "*branch_order_eq_ne<mode>"
  [(set (pc)
        (if_then_else
         (match_operator 1 "order_operator_eq_ne"
                 [(match_operand:GPR 2 "register_operand" "r")
                  (match_operand:GPR 3 "reg_or_imm5_operand" "rJYM")])
         (label_ref (match_operand 0 "" ""))
         (pc)))]
  "TARGET_PULP_BR"
{
  if (GET_CODE (operands[3]) == CONST_INT) {
    if ((INTVAL(operands[3]) != 0) && (INTVAL(operands[3])>=-16) && (INTVAL(operands[3])<=15)) {
        return "p.b%C1imm\t%2,%3,%0";
    } else return "b%C1z\t%2,%0";
  } return "b%C1\t%2,%3,%0";
}
  [(set_attr "type" "branch")
   (set_attr "mode" "none")])

