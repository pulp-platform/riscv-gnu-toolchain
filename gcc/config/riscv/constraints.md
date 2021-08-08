;; Constraint definitions for RISC-V target.
;; Copyright (C) 2011-2019 Free Software Foundation, Inc.
;; Contributed by Andrew Waterman (andrew@sifive.com).
;; Based on MIPS target for GNU compiler.
;;
;; PULP family support contributed by Eric Flamand (eflamand@iis.ee.ethz.ch) at ETH-Zurich
;; and Greenwaves Technologies (eric.flamand@greenwaves-technologies.com)
;;
;;
;; This file is part of GCC.
;;
;; GCC is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; GCC is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GCC; see the file COPYING3.  If not see
;; <http://www.gnu.org/licenses/>.


;; PULP Register constraints
(define_register_constraint "a" "TARGET_PULP_COMPAT ? GR_REGS : NO_REGS"
  "gp reg if pulpv0/v1, empty otherwise")

;; HW Loop register constraints, loop end
(define_register_constraint "t" "LE_REGS"
  "LE0 or LE1.")

;; HW Loop register constraints, loop start
(define_register_constraint "u" "LS_REGS"
  "LS0 or LS1.")

;; HW Loop register constraints, loop counter
(define_register_constraint "k" "LC_REGS"
  "LC0 or LC1.")

;; Register constraints

(define_register_constraint "f"
  "TARGET_HARD_FLOAT ? FP_REGS : ((TARGET_ZFINX || TARGET_ZDINX
  || TARGET_PULP_FHALFINX || TARGET_PULP_FALTHALFINX) ? GR_REGS : NO_REGS)"
  "A floating-point register (if available) or a X reg when zfinx/zdinx.")

(define_register_constraint "j" "SIBCALL_REGS"
  "@internal")

;; Avoid using register t0 for JALR's argument, because for some
;; microarchitectures that is a return-address stack hint.
(define_register_constraint "l" "JALR_REGS"
  "@internal")

;; Integer constraints

(define_constraint "Z"
  "@internal"
  (and (match_code "const_int")
       (match_test "1")))

;; General constraints

(define_constraint "I"
  "An I-type 12-bit signed immediate."
  (and (match_code "const_int")
       (match_test "SMALL_OPERAND (ival)")))

(define_constraint "J"
  "Integer zero."
  (and (match_code "const_int")
       (match_test "ival == 0")))

(define_constraint "K"
  "A 5-bit unsigned immediate for CSR access instructions."
  (and (match_code "const_int")
       (match_test "IN_RANGE (ival, 0, 31)")))

(define_constraint "L"
  "A U-type 20-bit signed immediate."
  (and (match_code "const_int")
       (match_test "LUI_OPERAND (ival)")))

;; PULP
(define_constraint "PULPL"
 "A 12-bit unsigned immediate."
  (and (match_code "const_int")
       (and (match_test "ival>=0") (match_test "ival<=4095"))))

;; Floating-point constant +0.0, used for FCVT-based moves when FMV is
;; not available in RV32.
(define_constraint "G"
  "@internal"
  (and (match_code "const_double")
       (match_test "op == CONST0_RTX (mode)")))

(define_memory_constraint "A"
  "An address that is held in a general-purpose register."
  (and (match_code "mem")
       (match_test "GET_CODE(XEXP(op,0)) == REG")))

(define_constraint "S"
  "@internal
   A constant call address."
  (match_operand 0 "absolute_symbolic_operand"))

(define_constraint "U"
  "@internal
   A PLT-indirect call address."
  (match_operand 0 "plt_symbolic_operand"))

(define_constraint "T"
  "@internal
   A constant @code{move_operand}."
  (and (match_operand 0 "move_operand")
       (match_test "CONSTANT_P (op)")))

;; PULP
(define_memory_constraint "YU"
  "A valid tiny memory operand"
  (and (match_code "mem")
       (match_test "tiny_memory_operand (op, VOIDmode)")))

(define_memory_constraint "W"
  "@internal
   A memory address based on a member of @code{BASE_REG_CLASS}."
  (and (match_code "mem")
       (match_operand 0 "memory_operand")))

(define_constraint "YG"
  "@internal
   A vector zero."
  (and (match_code "const_vector")
       (match_test "op == CONST0_RTX (mode)")))

(define_constraint "YM"
  "@internal"
  (and (match_code "const_int")
       (ior (match_test "(INTVAL(op) == 0)")
	    (match_test "(TARGET_PULP_BR && (INTVAL(op)>=-16) && (INTVAL(op)<=15))"))))

(define_constraint "vIsdc"
  "A constant vector with identical elements in [-32..31]"
   (and (match_code "const_vector")
        (match_test "riscv_replicated_const_vector(op, -32, 31)")))

(define_constraint "vIusc"
  "A constant vector with identical elements in [0..63]"
   (and (match_code "const_vector")
        (match_test "riscv_replicated_const_vector(op, 0, 63)")))

(define_constraint "vIzzz"
  "A constant vector with Floating-point zero elements."
  (and (match_code "const_vector")
       (match_test "op == CONST0_RTX (mode)")))

