/* Subroutines used for expanding RISC-V builtins.
   Copyright (C) 2011-2019 Free Software Foundation, Inc.
   Contributed by Andrew Waterman (andrew@sifive.com).

   PULP family support contributed by Eric Flamand (eflamand@iis.ee.ethz.ch) at ETH-Zurich
   and Greenwaves Technologies (eric.flamand@greenwaves-technologies.com)

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#define IN_TARGET_CODE 1

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "rtl.h"
#include "tree.h"
#include "gimple-expr.h"
#include "memmodel.h"
#include "expmed.h"
#include "profile-count.h"
#include "optabs.h"
#include "recog.h"
#include "diagnostic-core.h"
#include "stor-layout.h"
#include "expr.h"
#include "langhooks.h"
#include "explow.h"
#include "riscv-protos.h"

/* Macros to create an enumeration identifier for a function prototype.  */
#define RISCV_FTYPE_NAME1(A, B) RISCV_##A##_FTYPE_##B
#define RISCV_FTYPE_NAME2(A, B, C) RISCV_##A##_FTYPE_##B##_##C
#define RISCV_FTYPE_NAME3(A, B, C, D) RISCV_##A##_FTYPE_##B##_##C##_##D
#define RISCV_FTYPE_NAME4(A, B, C, D, E) RISCV_##A##_FTYPE_##B##_##C##_##D##_##E
#define RISCV_FTYPE_NAME5(A, B, C, D, E, F) RISCV_##A##_FTYPE_##B##_##C##_##D##_##E##_##F
#define RISCV_FTYPE_NAME6(A, B, C, D, E, F, G) RISCV_##A##_FTYPE_##B##_##C##_##D##_##E##_##F##_##G

/* Classifies the prototype of a built-in function.  */
enum riscv_function_type {
#define DEF_RISCV_FTYPE(NARGS, LIST) RISCV_FTYPE_NAME##NARGS LIST,
#include "config/riscv/riscv-ftypes.def"
#undef DEF_RISCV_FTYPE
  RISCV_MAX_FTYPE_MAX
};

/* Specifies how a built-in function should be converted into rtl.  */
enum riscv_builtin_type {
  /* The function corresponds directly to an .md pattern.  */
  RISCV_BUILTIN_DIRECT,

  /* Likewise, but with return type VOID.  */
  RISCV_BUILTIN_DIRECT_NO_TARGET
};

/* Declare an availability predicate for built-in functions.  */
#define AVAIL(NAME, COND)		\
 static unsigned int			\
 riscv_builtin_avail_##NAME (void)	\
 {					\
   return (COND);			\
 }

/* PULP */
struct PostExtractAction {
        int Yes;
        int Size;
        int Off;
        int Sign;
};

struct ExtraBuiltinImmArg {
        int Count;
        int IsReg[4];
        int Pos[4];
        int Value[4];
        struct PostExtractAction PostExtract;
};

typedef int (*BuiltinChecker)(int Code, int BuiltinIndex, struct ExtraBuiltinImmArg *ExtraImmArg, int Narg, ...);

/* This structure describes a single built-in function.  */
struct riscv_builtin_description {
  /* The code of the main .md file instruction.  See riscv_builtin_type
     for more information.  */
  enum insn_code icode;

  /* The name of the built-in function.  */
  const char *name;

  /* Specifies how the function should be expanded.  */
  enum riscv_builtin_type builtin_type;

  /* The function's prototype.  */
  enum riscv_function_type prototype;

  /* Whether the function is available.  */
  unsigned int (*avail) (void);

  /* Whether the function args are correct, optional */
  BuiltinChecker check;
};

AVAIL (hard_float, TARGET_HARD_FLOAT)

/* PULP builtins CHECKs */
AVAIL (pulp_bitop_small, TARGET_PULP_BITOP_SMALL)
AVAIL (pulp_mac_alt, TARGET_PULP_MAC_ALT)

AVAIL (pulp_postmod, TARGET_PULP_POSTMOD)
AVAIL (pulp_indregreg, TARGET_PULP_INDREGREG)
AVAIL (pulp_addressing, (TARGET_PULP_POSTMOD || TARGET_PULP_INDREGREG))
AVAIL (pulp_abs, TARGET_PULP_ABS)
AVAIL (pulp_slet, TARGET_PULP_SLET)
AVAIL (pulp_minmax, TARGET_PULP_MINMAX)
AVAIL (pulp_bitop, TARGET_PULP_BITOP)
AVAIL (pulp_bitop_small_and_full, (TARGET_PULP_BITOP_SMALL || TARGET_PULP_BITOP))
AVAIL (pulp_clip, TARGET_PULP_CLIP)
AVAIL (pulp_hwloop, TARGET_PULP_HWLOOP)
AVAIL (pulp_mac_si, TARGET_PULP_MAC_SI)
AVAIL (pulp_macrn_hi, TARGET_PULP_MACRN_HI)
AVAIL (pulp_mulrn_hi, TARGET_PULP_MULRN_HI)
AVAIL (pulp_partmac, TARGET_PULP_PARTMAC)
AVAIL (pulp_addsubrn, TARGET_PULP_ADDSUBRN)
AVAIL (pulp_vect, TARGET_PULP_VECT)
AVAIL (pulp_vect_shufflepack, TARGET_PULP_VECT_SHUFFLEPACK)
AVAIL (pulp_br, TARGET_PULP_BR)
AVAIL (pulp_elw, TARGET_PULP_ELW)

AVAIL (pulp_vect_complex, TARGET_PULP_VECT_COMPLEX)
AVAIL (pulp_vect_gap8, TARGET_PULP_VECT_GAP8)

AVAIL (pulp_any, (pulp_target_flags != 0))

/* legacy */
AVAIL (pulp_gap8_only, (Pulp_Cpu == PULP_GAP8))

/* for builtin pulpv2, we model vectors as opaque entities. Opaque helps to make the call style mode versatile */
static tree opaque_V4QI_type_node;
static tree opaque_V2HI_type_node;

static unsigned int
riscv_builtin_avail_riscv (void)
{
  return 1;
}


static int CheckBuiltin(int Code, int BuiltinIndex, struct ExtraBuiltinImmArg *ExtraImmArg, int Narg, ...);


/* Construct a riscv_builtin_description from the given arguments.

   INSN is the name of the associated instruction pattern, without the
   leading CODE_FOR_riscv_.

   NAME is the name of the function itself, without the leading
   "__builtin_riscv_".

   BUILTIN_TYPE and FUNCTION_TYPE are riscv_builtin_description fields.

   AVAIL is the name of the availability predicate, without the leading
   riscv_builtin_avail_.  */

#define RISCV_BUILTIN(INSN, NAME, BUILTIN_TYPE, FUNCTION_TYPE, AVAIL, CHECK)    \
  { CODE_FOR_riscv_ ## INSN, "__builtin_riscv_" NAME,                         \
    BUILTIN_TYPE, FUNCTION_TYPE, riscv_builtin_avail_ ## AVAIL , CHECK },

#define RISCV_BUILTIN1(INSN, NAME, BUILTIN_TYPE, FUNCTION_TYPE, AVAIL, CHECK)   \
  { CODE_FOR_ ## INSN, "__builtin_pulp_" NAME,                          \
    BUILTIN_TYPE, FUNCTION_TYPE, riscv_builtin_avail_ ## AVAIL , CHECK },

#define RISCV_BUILTIN2(INSN, NAME, BUILTIN_TYPE, FUNCTION_TYPE, AVAIL, CHECK)   \
  { CODE_FOR_ ## INSN, "__builtin_" NAME,                               \
    BUILTIN_TYPE, FUNCTION_TYPE, riscv_builtin_avail_ ## AVAIL , CHECK },

/* Define __builtin_riscv_<INSN>, which is a RISCV_BUILTIN_DIRECT function
   mapped to instruction CODE_FOR_riscv_<INSN>,  FUNCTION_TYPE and AVAIL
   are as for RISCV_BUILTIN.  */
#define DIRECT_BUILTIN(INSN, FUNCTION_TYPE, AVAIL, CHECK)                       \
  RISCV_BUILTIN (INSN, #INSN, RISCV_BUILTIN_DIRECT, FUNCTION_TYPE, AVAIL, CHECK)

#define DIRECT_BUILTIN1(INSN, NAME, FUNCTION_TYPE, AVAIL, CHECK)                        \
  RISCV_BUILTIN1 (INSN, #NAME, RISCV_BUILTIN_DIRECT, FUNCTION_TYPE, AVAIL, CHECK)

/* Define __builtin_riscv_<INSN>, which is a RISCV_BUILTIN_DIRECT_NO_TARGET
   function mapped to instruction CODE_FOR_riscv_<INSN>,  FUNCTION_TYPE
   and AVAIL are as for RISCV_BUILTIN.  */
#define DIRECT_NO_TARGET_BUILTIN(INSN, FUNCTION_TYPE, AVAIL, CHECK)             \
  RISCV_BUILTIN (INSN, #INSN, RISCV_BUILTIN_DIRECT_NO_TARGET,           \
                FUNCTION_TYPE, AVAIL, CHECK)

#define DIRECT_NO_TARGET_BUILTIN1(INSN, NAME, FUNCTION_TYPE, AVAIL, CHECK)                      \
  RISCV_BUILTIN1 (INSN, #NAME, RISCV_BUILTIN_DIRECT_NO_TARGET, FUNCTION_TYPE, AVAIL, CHECK)

/* Argument types.  */
#define RISCV_ATYPE_VOID void_type_node
#define RISCV_ATYPE_USI unsigned_intSI_type_node

/* RISCV_FTYPE_ATYPESN takes N RISCV_FTYPES-like type codes and lists
   their associated RISCV_ATYPEs.  */
#define RISCV_FTYPE_ATYPES1(A, B) \
  RISCV_ATYPE_##A, RISCV_ATYPE_##B

static const struct riscv_builtin_description riscv_builtins[] = {
  DIRECT_BUILTIN (frflags, RISCV_USI_FTYPE_VOID, hard_float, NULL)
  DIRECT_NO_TARGET_BUILTIN (fsflags, RISCV_VOID_FTYPE_USI, hard_float, NULL)
#include "pulp-builtins.def"
};

#undef DIRECT_NO_TARGET_BUILTIN1
#define DIRECT_NO_TARGET_BUILTIN1(INSN, NAME, FUNCTION_TYPE, AVAIL, CHECK) PULP_BUILTIN_ ## NAME,
#undef DIRECT_NO_TARGET_BUILTIN
#define DIRECT_NO_TARGET_BUILTIN(INSN, FUNCTION_TYPE, AVAIL, CHECK) PULP_BUILTIN_ ## NAME,
#undef DIRECT_BUILTIN1
#define DIRECT_BUILTIN1(INSN, NAME, FUNCTION_TYPE, AVAIL, CHECK) PULP_BUILTIN_ ## NAME,
enum Pulp_Builtin_Id {
RISCV_BUILTIN_frflags,
RISCV_BUILTIN_fsflags,	
#include "pulp-builtins.def"
};

static int CheckBuiltin(int Code, int BuiltinIndex, struct ExtraBuiltinImmArg *ExtraImmArg, int Narg, ...)

{
	int i;
	rtx Op[6] = {NULL, NULL, NULL, NULL, NULL, NULL};
	const char *Diag=NULL;
	va_list ap;

	va_start(ap, Narg);
	for (i=0; i<Narg; i++) Op[i] = va_arg(ap, rtx);
	va_end(ap);

	if (ExtraImmArg) {
		ExtraImmArg->Count = 0;
		ExtraImmArg->PostExtract.Yes = 0;
	}

	switch ((enum Pulp_Builtin_Id) BuiltinIndex) {
		case PULP_BUILTIN_CoreCount:
			ExtraImmArg->Count = 2;
			ExtraImmArg->Pos[0] = 1; ExtraImmArg->Pos[1] = 2;
			ExtraImmArg->IsReg[0] = 1; ExtraImmArg->IsReg[1] = 0;
			// Gap8: (APB_SOC_CTRL_ADDR + 0x12) = SOC_PERIPHERALS_BASE_ADDR + 0x0300 + 0x12
			// 0x1A100000 + 0x3000 + 0x12
			ExtraImmArg->Value[0] = 0x1A103000;
			ExtraImmArg->Value[1] = 0x12;
			break;
		case PULP_BUILTIN_CoreId:
			ExtraImmArg->Count = 1;
			ExtraImmArg->IsReg[0] = 0;
			if (Pulp_Cpu>=PULP_V2) ExtraImmArg->Value[0] = 0xF14;
			else ExtraImmArg->Value[0] = 0xF10;
			ExtraImmArg->PostExtract.Yes = 1; ExtraImmArg->PostExtract.Size = 5;
			ExtraImmArg->PostExtract.Off = 0; ExtraImmArg->PostExtract.Sign = 0;
			Op[0] = gen_rtx_CONST_INT(SImode, ExtraImmArg->Value[0]);
			break;
		case PULP_BUILTIN_ClusterId:
			ExtraImmArg->Count = 1;
			ExtraImmArg->IsReg[0] = 0;
			if (Pulp_Cpu>=PULP_V2) ExtraImmArg->Value[0] = 0xF14;
			else ExtraImmArg->Value[0] = 0xF10;
			ExtraImmArg->PostExtract.Yes = 1; ExtraImmArg->PostExtract.Size = 6;
			ExtraImmArg->PostExtract.Off = 5; ExtraImmArg->PostExtract.Sign = 0;
			Op[0] = gen_rtx_CONST_INT(SImode, ExtraImmArg->Value[0]);
			break;
		case PULP_BUILTIN_IsFc:
			ExtraImmArg->Count = 1;
			ExtraImmArg->IsReg[0] = 0;
			if (Pulp_Cpu>=PULP_V2) ExtraImmArg->Value[0] = 0xF14;
			else ExtraImmArg->Value[0] = 0xF10;
			ExtraImmArg->PostExtract.Yes = 1; ExtraImmArg->PostExtract.Size = 1;
			ExtraImmArg->PostExtract.Off = 10; ExtraImmArg->PostExtract.Sign = 1;
			Op[0] = gen_rtx_CONST_INT(SImode, ExtraImmArg->Value[0]);
			break;
		default:
			break;
	}

	switch (Code) {
		/* Op3 const and in 0..31, Op4 const and == 2^(Op3 - 1) */
		case CODE_FOR_macsRNr_si3:
		case CODE_FOR_macuRNr_si3:
		case CODE_FOR_machhsRNr_si3:
		case CODE_FOR_machhuRNr_si3:
			if (Op[3] && (GET_CODE(Op[3]) == CONST_INT) && Op[4] && (GET_CODE(Op[4]) == CONST_INT)) {
				int Norm = INTVAL (Op[3]);
				int Round = INTVAL (Op[4]);
				if (Norm >= 0 && Norm <= 31) {
					if ((1 << (Norm - 1)) == Round) return 1;
				}
			}
			Diag = "__builtin_pulp_mac{hh,}{s,u}NRr (X, Y, Acc, Norm, Round) expects Norm and Round cst, Norm<=31, Round==2^(Norm-1)";
			break;
		/* Op2 const and in 0..31, Op3 const and == 2^(Op2 - 1) */
		case CODE_FOR_mulsRNr_hi3:
		case CODE_FOR_mulsRNr_si3:
		case CODE_FOR_muluRNr_si3:
		case CODE_FOR_mulhhsRNr_si3:
		case CODE_FOR_mulhhuRNr_si3:
			Diag = "__builtin_pulp_mul{hh,}{s,u}NRr (X, Y, Norm, Round) expects Norm and Round cst, Norm<=31, Round==2^(Norm-1)";
		case CODE_FOR_addRN_si3:
		case CODE_FOR_addRNu_si3:
		case CODE_FOR_subRN_si3:
		case CODE_FOR_subRNu_si3:
			if (Op[2] && (GET_CODE(Op[2]) == CONST_INT) && Op[3] && (GET_CODE(Op[3]) == CONST_INT)) {
				int Norm = INTVAL (Op[2]);
				int Round = INTVAL (Op[3]);
				if (Norm >= 0 && Norm <= 31) {
					if ((1 << (Norm - 1)) == Round) return 1;
				}
			}
			if (Diag == NULL)
				Diag = "__builtin_pulp_{add,addu,sub}{hh,}NRr (X, Y, Norm, Round) expects Norm and Round cst, Norm<=31, Round==2^(Norm-1)";
			break;
		/* Op3 const and in 0..31 */
		case CODE_FOR_macsNr_si3:
		case CODE_FOR_macuNr_si3:
		case CODE_FOR_machhsNr_si3:
		case CODE_FOR_machhuNr_si3:
			if (Op[3] && (GET_CODE(Op[3]) == CONST_INT)) {
				int Norm = INTVAL (Op[3]);
				if (Norm >= 0 && Norm <= 31) return 1;
			}
			Diag = "__builtin_pulp_mac{hh,}{s,u}NRr (X, Y, Acc, Norm, Round) expects Norm and Round cst, Norm<=31, Round==2^(Norm-1)";
			break;
		/* Op2 const and in 0..31 */
		case CODE_FOR_mulsNr_si3:
		case CODE_FOR_muluNr_si3:
		case CODE_FOR_mulhhsNr_si3:
		case CODE_FOR_mulhhuNr_si3:
			Diag = "__builtin_pulp_mul{hh,}{s,u}Nr (X, Y, Norm) expects Norm cst, Norm<=31";
		case CODE_FOR_addN_si3:
		case CODE_FOR_addNu_si3:
		case CODE_FOR_subN_si3:
		case CODE_FOR_subNu_si3:
			if (Op[2] && (GET_CODE(Op[2]) == CONST_INT)) {
				int Norm = INTVAL (Op[2]);
				if (Norm >= 0 && Norm <= 31) return 1;
			}
			if (Diag==NULL) Diag = "__builtin_pulp_{add,addu,sub}Nr (X, Y, Norm) expects Norm cst, Norm<=31";
			break;
		/* Op1 const, Op2 const, Op1==-2^(N-1), Op2==2^(N-1)-1 */
		case CODE_FOR_clip_minmax:
			if (Op[1] && (GET_CODE(Op[1]) == CONST_INT) && Op[2] && (GET_CODE(Op[2]) == CONST_INT)) {
				int i;
				int Min = INTVAL (Op[1]);
				int Max = INTVAL (Op[2]);
				for (i = 0; i < 30; i ++) if ((Max == (1 << i) - 1) && (Min == - (1 << i))) return 1;
			}
			Diag = "__builtin_pulp_clip (X, Min, Max) expects Min and Max cst, Min=-2^(N-1), Max=2^(N-1)-1";
			break;
		/* Op1 const, Op2 const, Op1==0, Op2==2^(N-1)-1 */
		case CODE_FOR_clipu_minmax:
			if (Op[1] && (GET_CODE(Op[1]) == CONST_INT) && Op[2] && (GET_CODE(Op[2]) == CONST_INT)) {
				int i;
				int Min = INTVAL (Op[1]);
				int Max = INTVAL (Op[2]);
				if (Min==0) for (i = 0; i < 30; i ++) if ((Max == (1 << i) - 1)) return 1;
			}
			Diag = "__builtin_pulp_clipu (X, Min, Max) expects Min and Max cst, Min=0, Max=2^(N-1)-1";
			break;
		case CODE_FOR_bclrsi3:
			if (Op[1] && (GET_CODE(Op[1]) == CONST_INT)) {
				if (riscv_valid_bit_field_imm_operand(Op[1], NULL, 0, NULL, NULL)) return 1;
			}
			Diag = "builtin_pulp_bclr(X, BitMask) expects BistMask cst, BitMask = ~(BitToClear)\n";
			break;
		case CODE_FOR_bsetsi3:
			if (Op[1] && (GET_CODE(Op[1]) == CONST_INT)) {
				if (riscv_valid_bit_field_imm_operand(Op[1], NULL, 1, NULL, NULL)) return 1;
			}
			Diag = "builtin_pulp_bset(X, BitMask) expects BistMask cst, BitMask = BitToSet\n";
			break;
		/* Op1 const > 0, Op2 const >= 0, (Op1+Op2)<32 */
		case CODE_FOR_extvsi:
		case CODE_FOR_extzvsi:
			if (Op[1] && (GET_CODE(Op[1]) == CONST_INT) && Op[2] && (GET_CODE(Op[2]) == CONST_INT)) {
				int Size   = INTVAL (Op[1]);
				int Offset = INTVAL (Op[2]);
				if (Size > 0 && Offset >=0 && ((Size+Offset)<=32)) return 1;
			}
			Diag = "__builtin_pulp_bextract(X, Size, Offset) Expects Size and Offset immediate constants, Size>0, Offset>=0, (Size+Offset)<=32";
			break;
		/* Op0 const > 0, Op1 const >= 0, (Op0+Op1)<32 */
		/* Op0 -> Target
		   Op1 -> ~Mask
		   Op2 -> InsVal
		   Op3 -> Mask
		   Op4 -> Off
		*/
		case CODE_FOR_invsipat1:
			if (Op[1] && (GET_CODE(Op[1]) == CONST_INT) && Op[3] && (GET_CODE(Op[3]) == CONST_INT) &&
			    Op[4] && (GET_CODE(Op[4]) == CONST_INT)) {
				unsigned int Mask   = UINTVAL (Op[3]);
				unsigned int MaskBar= UINTVAL (Op[1]);
				unsigned int Size=0;
				unsigned int Offset = UINTVAL (Op[4]);
				int i;
				for (i=Offset; i<32; i++, Size++) if (((1<<i) & Mask) == 0) break;
				if ((MaskBar == ~Mask) && (Offset <= 31) &&
				    ((unsigned int) (((1<<Size) - 1) << Offset) == Mask)) return 1;
			}
			Diag = "__builtin_pulp_binsert (Target, MaskBar, InsVal, Size, Mask, Off) expects Off,Mask,MaskBar cst, Size>0, Off>=0, (Off+Size)<=32";
			break;
		case CODE_FOR_load_evt_unit:
			if (Op[1] && (GET_CODE(Op[1]) == CONST_INT)) return 1;
			Diag = "__builtin_event_unit_read(base, offset), offset expected to be immediate value";
			break;
		case CODE_FOR_read_spr_vol:
		case CODE_FOR_read_spr:
			if (Op[0] && (GET_CODE(Op[0]) == CONST_INT)) {
				unsigned int Reg = UINTVAL(Op[0]);
				if (Reg <= 4091) return 1;
			}
			Diag = "__builtin_pulp_spr_read(Spr) or __builtin_pulp_spr_read_vol(Spr) expects Spr to be immediate and in [0..4091]";
			break;
		case CODE_FOR_write_spr:
			if (Op[0] && (GET_CODE(Op[0]) == CONST_INT)) {
				unsigned int Reg = UINTVAL(Op[0]);
				if (Reg <= 4091) return 1;
			}
			Diag = "__builtin_pulp_spr_write(Spr, Value) expects Spr to be immediate and in [0..4091]";
			break;
		case CODE_FOR_read_then_write_spr:
			if (Op[0] && (GET_CODE(Op[0]) == CONST_INT)) {
				unsigned int Reg = UINTVAL(Op[0]);
				if (Reg <= 4091) return 1;
			}
			Diag = "__builtin_pulp_read_then_spr_write(Spr, Value) expects Spr to be immediate and in [0..4091]";
			break;
		case CODE_FOR_spr_bit_set:
			if (Op[0] && (GET_CODE(Op[0]) == CONST_INT)) {
				unsigned int Reg = UINTVAL(Op[0]);
				if (Reg <= 4091) return 1;
			}
			Diag = "__builtin_pulp_spr_bit_set(Spr, Value) expects Spr to be immediate and in [0..4091]";
			break;
		case CODE_FOR_read_then_spr_bit_set:
			if (Op[0] && (GET_CODE(Op[0]) == CONST_INT)) {
				unsigned int Reg = UINTVAL(Op[0]);
				if (Reg <= 4091) return 1;
			}
			Diag = "__builtin_pulp_read_then_spr_bit_set(Spr, Value) expects Spr to be immediate and in [0..4091]";
			break;
		case CODE_FOR_spr_bit_clr:
			if (Op[0] && (GET_CODE(Op[0]) == CONST_INT)) {
				unsigned int Reg = UINTVAL(Op[0]);
				if (Reg <= 4091) return 1;
			}
			Diag = "__builtin_pulp_spr_bit_clr(Spr, Value) expects Spr to be immediate and in [0..4091]";
			break;
		case CODE_FOR_read_then_spr_bit_clr:
			if (Op[0] && (GET_CODE(Op[0]) == CONST_INT)) {
				unsigned int Reg = UINTVAL(Op[0]);
				if (Reg <= 4091) return 1;
			}
			Diag = "__builtin_pulp_read_then_spr_bit_clr(Spr, Value) expects Spr to be immediate and in [0..4091]";
			break;
		/* Internal error no handler for this builtin code */
		default:
			// gcc_unreachable ();
			return 1;


	}
	/* Wrong arguments passed to builtin */
	if (Diag) error("Builtin %s", Diag); else error("Builtin No Diagnosis");
	return 0;
}

/* Index I is the function declaration for riscv_builtins[I], or null if the
   function isn't defined on this target.  */
static GTY(()) tree riscv_builtin_decls[ARRAY_SIZE (riscv_builtins)];

/* Get the index I of the function declaration for riscv_builtin_decls[I]
   using the instruction code or return null if not defined for the target.  */
static GTY(()) int riscv_builtin_decl_index[NUM_INSN_CODES];

#define GET_BUILTIN_DECL(CODE) \
  riscv_builtin_decls[riscv_builtin_decl_index[(CODE)]]

/* Source-level argument types.  */
#define RISCV_ATYPE_VOID void_type_node
#define RISCV_ATYPE_CHAR char_type_node
#define RISCV_ATYPE_SHORT short_integer_type_node
#define RISCV_ATYPE_INT integer_type_node
#define RISCV_ATYPE_POINTER ptr_type_node
#define RISCV_ATYPE_CPOINTER const_ptr_type_node

/* Standard mode-based argument types.  */
#define RISCV_ATYPE_UQI unsigned_intQI_type_node
#define RISCV_ATYPE_SI intSI_type_node
#define RISCV_ATYPE_USI unsigned_intSI_type_node
#define RISCV_ATYPE_DI intDI_type_node
#define RISCV_ATYPE_UDI unsigned_intDI_type_node
#define RISCV_ATYPE_SF float_type_node
#define RISCV_ATYPE_DF double_type_node

#define RISCV_ATYPE_V2HI opaque_V2HI_type_node
#define RISCV_ATYPE_V4QI opaque_V4QI_type_node

/* RISCV_FTYPE_ATYPESN takes N RISCV_FTYPES-like type codes and lists
   their associated RISCV_ATYPEs.  */
#define RISCV_FTYPE_ATYPES1(A, B) \
  RISCV_ATYPE_##A, RISCV_ATYPE_##B

#define RISCV_FTYPE_ATYPES2(A, B, C) \
  RISCV_ATYPE_##A, RISCV_ATYPE_##B, RISCV_ATYPE_##C

#define RISCV_FTYPE_ATYPES3(A, B, C, D) \
  RISCV_ATYPE_##A, RISCV_ATYPE_##B, RISCV_ATYPE_##C, RISCV_ATYPE_##D

#define RISCV_FTYPE_ATYPES4(A, B, C, D, E) \
  RISCV_ATYPE_##A, RISCV_ATYPE_##B, RISCV_ATYPE_##C, RISCV_ATYPE_##D, \
  RISCV_ATYPE_##E

#define RISCV_FTYPE_ATYPES5(A, B, C, D, E, F) \
  RISCV_ATYPE_##A, RISCV_ATYPE_##B, RISCV_ATYPE_##C, RISCV_ATYPE_##D, \
  RISCV_ATYPE_##E, RISCV_ATYPE_##F

#define RISCV_FTYPE_ATYPES6(A, B, C, D, E, F, G) \
  RISCV_ATYPE_##A, RISCV_ATYPE_##B, RISCV_ATYPE_##C, RISCV_ATYPE_##D, \
  RISCV_ATYPE_##E, RISCV_ATYPE_##F, RISCV_ATYPE_##G

/* Return the function type associated with function prototype TYPE.  */

#define MAX_REMAPPED_GOMP 20
static struct {
	unsigned int Gomp;
	unsigned int Pulp;
} Remapped_GOMP_Builtins[MAX_REMAPPED_GOMP];

static int Head_Remapped_GOMP_Builtins=0;

enum Riscv_Native_GOMP_Builtins {
	NATIVE_GOMP_LOOP_CHUNK_SIZE = 0,
	NATIVE_GOMP_LOOP_START = 1,
	NATIVE_GOMP_LAST = 2
};

static struct {
	tree	TypeDescr;
	int	Base;
	int 	Index;
} Native_GOMP_Builtins[NATIVE_GOMP_LAST] = 
{
	{NULL, 0x00204000, 0x70},		// OMP Loop Chunk Size
	{NULL, 0x00204000, 0x64},		// OMP Loop Start
};

unsigned int GetRemappedGompBuiltin(unsigned int ompcode, unsigned int def_ret)

{
	int i;

	for (i=0; i<Head_Remapped_GOMP_Builtins; i++) {
		if (Remapped_GOMP_Builtins[i].Gomp == ompcode) return Remapped_GOMP_Builtins[i].Pulp;
	}
	return def_ret ;
}


/* Return the function type associated with function prototype TYPE.  */

static tree
riscv_build_function_type (enum riscv_function_type type)
{
  static tree types[(int) RISCV_MAX_FTYPE_MAX];

  if (types[(int) type] == NULL_TREE)
    switch (type)
      {
#define DEF_RISCV_FTYPE(NUM, ARGS)					\
  case RISCV_FTYPE_NAME##NUM ARGS:					\
    types[(int) type]							\
      = build_function_type_list (RISCV_FTYPE_ATYPES##NUM ARGS,		\
				  NULL_TREE);				\
    break;
#include "config/riscv/riscv-ftypes.def"
#undef DEF_RISCV_FTYPE
      default:
	gcc_unreachable ();
      }

  return types[(int) type];
}

/* Implement TARGET_INIT_BUILTINS.  */

void
riscv_init_builtins (void)
{
/*
  tree fp16_type = make_node (REAL_TYPE);
  tree fp8_type = make_node (REAL_TYPE);

  TYPE_PRECISION (fp16_type) = 16; TYPE_PRECISION (fp8_type) = 8;
  layout_type (fp16_type); layout_type (fp8_type);
  (*lang_hooks.types.register_builtin_type) (fp16_type, "__fp16");
  (*lang_hooks.types.register_builtin_type) (fp8_type, "__fp8");
*/

  opaque_V4QI_type_node    = build_opaque_vector_type (intQI_type_node, 4);
  opaque_V2HI_type_node    = build_opaque_vector_type (intHI_type_node, 2);

  /* Iterate through all of the bdesc arrays, initializing all of the
     builtin functions.  */
  for (size_t i = 0; i < ARRAY_SIZE (riscv_builtins); i++)
    {
      const struct riscv_builtin_description *d = &riscv_builtins[i];
      if (d->avail ()) {

        /* fprintf(stderr, "Adding %s\n", d->name); fflush(stderr); */
                riscv_builtin_decls[i] = add_builtin_function (d->name,
                                                               riscv_build_function_type (d->prototype),
                                                               i, BUILT_IN_MD, NULL, NULL);
	  	riscv_builtin_decl_index[d->icode] = i;
                switch (d->icode) {
                        case CODE_FOR_pulp_omp_barrier:
                                Remapped_GOMP_Builtins[Head_Remapped_GOMP_Builtins].Gomp = BUILT_IN_GOMP_BARRIER;
                                Remapped_GOMP_Builtins[Head_Remapped_GOMP_Builtins].Pulp = i;
                                Head_Remapped_GOMP_Builtins++;
                                break;
                        case CODE_FOR_pulp_omp_critical_start:
                                Remapped_GOMP_Builtins[Head_Remapped_GOMP_Builtins].Gomp = BUILT_IN_GOMP_CRITICAL_START;
                                Remapped_GOMP_Builtins[Head_Remapped_GOMP_Builtins].Pulp = i;
                                Head_Remapped_GOMP_Builtins++;
                                break;
                        case CODE_FOR_pulp_omp_critical_end:
                                Remapped_GOMP_Builtins[Head_Remapped_GOMP_Builtins].Gomp = BUILT_IN_GOMP_CRITICAL_END;
                                Remapped_GOMP_Builtins[Head_Remapped_GOMP_Builtins].Pulp = i;
                                Head_Remapped_GOMP_Builtins++;
                                break;
                        case CODE_FOR_OffsetedReadOMP:
                                Native_GOMP_Builtins[NATIVE_GOMP_LOOP_CHUNK_SIZE].TypeDescr = riscv_builtin_decls[i];
                                Native_GOMP_Builtins[NATIVE_GOMP_LOOP_START].TypeDescr = riscv_builtin_decls[i];
                                break;
                        default:
                        ;
                }
       }
    }
}

/* Implement `TARGET_FOLD_BUILTIN'.  */

tree
riscv_fold_builtin (tree fndecl, int n_args ATTRIBUTE_UNUSED, tree *arg ATTRIBUTE_UNUSED,
                    bool ignore ATTRIBUTE_UNUSED)
{
        unsigned int fcode = DECL_FUNCTION_CODE (fndecl);

        switch (fcode) {
                case PULP_BUILTIN_CoreCount:
                        if (_Pulp_PE>0) {
                                 return build_int_cst (integer_type_node, _Pulp_PE);
                        } else return NULL_TREE;
                        break;
                case PULP_BUILTIN_HasFc:
                        if (_Pulp_FC) {
                                return integer_one_node; // build_int_cst (integer_type_node, 1);
                        } else {
                                return integer_zero_node; // build_int_cst (integer_type_node, 0);
                        }
                        break;
                case PULP_BUILTIN_IsFc:
                        if (_Pulp_FC == 0) return integer_zero_node; else return NULL_TREE;
                        break;
                default:
                        return NULL_TREE;
        }

}

/* Implement TARGET_BUILTIN_DECL.  */

tree
riscv_builtin_decl (unsigned int code, bool initialize_p ATTRIBUTE_UNUSED)
{
  if (code >= ARRAY_SIZE (riscv_builtins))
    return error_mark_node;
  return riscv_builtin_decls[code];
}

/* Take argument ARGNO from EXP's argument list and convert it into
   an expand operand.  Store the operand in *OP.  */

static rtx
riscv_prepare_builtin_arg (enum insn_code icode,
                          unsigned int opno, tree exp, unsigned int argno)
{
  tree arg;
  rtx value;
  enum machine_mode mode;

  arg = CALL_EXPR_ARG (exp, argno);
  value = expand_normal (arg);
  mode = insn_data[icode].operand[opno].mode;
  if (!insn_data[icode].operand[opno].predicate (value, mode))
    {
      /* We need to get the mode from ARG for two reasons:

           - to cope with address operands, where MODE is the mode of the
             memory, rather than of VALUE itself.

           - to cope with special predicates like pmode_register_operand,
             where MODE is VOIDmode.  */

      value = copy_to_mode_reg (TYPE_MODE (TREE_TYPE (arg)), value);


      /* Check the predicate again.  */
      if (!insn_data[icode].operand[opno].predicate (value, mode))
        {
// debug_rtx(value);
          error ("invalid argument to built-in function on arg %d, builtin arg mode: %s, actual arg mode: %s",
                  opno, GET_MODE_NAME(mode), GET_MODE_NAME(TYPE_MODE (TREE_TYPE (arg))));
          return const0_rtx;
        }
    }

  return value;
}

/* Return an rtx suitable for output operand OP of instruction ICODE.
   If TARGET is non-null, try to use it where possible.  */

static rtx
riscv_prepare_builtin_target (enum insn_code icode, unsigned int op, rtx target)
{
  enum machine_mode mode;

  mode = insn_data[icode].operand[op].mode;
  if (target == 0 || !insn_data[icode].operand[op].predicate (target, mode))
    target = gen_reg_rtx (mode);

  return target;
}

static void PulpBuiltinGenPostExtract(struct ExtraBuiltinImmArg *ExtraArg, rtx OutReg)

{
        if (!ExtraArg->PostExtract.Yes) return;

        if (ExtraArg->PostExtract.Sign)
                emit_insn(gen_extvsi(OutReg, OutReg, gen_rtx_CONST_INT(SImode, ExtraArg->PostExtract.Size),
                                                     gen_rtx_CONST_INT(SImode, ExtraArg->PostExtract.Off)));
        else
                emit_insn(gen_extzvsi(OutReg, OutReg, gen_rtx_CONST_INT(SImode, ExtraArg->PostExtract.Size),
                                                      gen_rtx_CONST_INT(SImode, ExtraArg->PostExtract.Off)));
}

/* Expand instruction ICODE as part of a built-in function sequence.
   Use the first NOPS elements of OPS as the instruction's operands.
   HAS_TARGET_P is true if operand 0 is a target; it is false if the
   instruction has no target.

   Return the target rtx if HAS_TARGET_P, otherwise return const0_rtx.  */

static rtx
riscv_expand_builtin_insn (enum insn_code icode, unsigned int n_ops,
			   struct expand_operand *ops, bool has_target_p)
{
  if (!maybe_expand_insn (icode, n_ops, ops))
    {
      error ("invalid argument to built-in function");
      return has_target_p ? gen_reg_rtx (ops[0].mode) : const0_rtx;
    }

  return has_target_p ? ops[0].value : const0_rtx;
}

/* Expand a RISCV_BUILTIN_DIRECT or RISCV_BUILTIN_DIRECT_NO_TARGET function;
   HAS_TARGET_P says which.  EXP is the CALL_EXPR that calls the function
   and ICODE is the code of the associated .md pattern.  TARGET, if nonnull,
   suggests a good place to put the result.  */


static rtx
riscv_expand_builtin_direct (const struct riscv_builtin_description *d, int builtin_index, enum insn_code icode, rtx target, tree exp,
                            bool has_target_p)
{
  rtx ops[MAX_RECOG_OPERANDS];
  int opno, argno;
  struct ExtraBuiltinImmArg ExtraArg;

  /* Map any target to operand 0.  */
  ExtraArg.Count = 0; ExtraArg.PostExtract.Yes = 0;
  opno = 0;
  if (has_target_p)
    {
      target = riscv_prepare_builtin_target (icode, opno, target);
      ops[opno] = target;
      opno++;
    }

  /* Map the arguments to the other operands.  The n_operands value
     for an expander includes match_dups and match_scratches as well as
     match_operands, so n_operands is only an upper bound on the number
     of arguments to the expander function.  */

  gcc_assert (opno + call_expr_nargs (exp) <= insn_data[icode].n_operands);
  for (argno = 0; argno < call_expr_nargs (exp); argno++, opno++)
    ops[opno] = riscv_prepare_builtin_arg (icode, opno, exp, argno);
  if (has_target_p) {
        if (d->check) {
                d->check(icode, builtin_index, &ExtraArg, call_expr_nargs (exp), ops[1], ops[2], ops[3], ops[4], ops[5]);
                if (ExtraArg.Count) {
                        int i;
                        for (i=0; i<ExtraArg.Count; i++) {
                                if (ExtraArg.IsReg[i]) {
                                        rtx Reg = gen_reg_rtx (SImode);

                                        emit_insn(gen_movsi(Reg, gen_rtx_CONST_INT(SImode, ExtraArg.Value[i])));
                                        ops[opno] = Reg;
                                } else {
                                        ops[opno] = gen_rtx_CONST_INT(SImode, ExtraArg.Value[i]);
                                }
                                opno++; argno++;
                        }
                }
        }
  } else {
        if (d->check) {
                d->check(icode, builtin_index, &ExtraArg, call_expr_nargs (exp), ops[0], ops[1], ops[2], ops[3]);
                if (ExtraArg.Count) {
                        int i;
                        for (i=0; i<ExtraArg.Count; i++) {
                                if (ExtraArg.IsReg[i]) {
                                        rtx Reg = gen_reg_rtx (SImode);

                                        emit_insn(gen_movsi(Reg, gen_rtx_CONST_INT(SImode, ExtraArg.Value[i])));
                                        ops[opno] = Reg;
                                } else {
                                        ops[opno] = gen_rtx_CONST_INT(SImode, ExtraArg.Value[i]);
                                }
                                opno++; argno++;
                        }
                }
        }
  }
  switch (opno)
    {
    case 0:
      emit_insn (GEN_FCN (icode) ());
      break;
    case 1:
      emit_insn (GEN_FCN (icode) (ops[0]));
      break;
    case 2:
      emit_insn (GEN_FCN (icode) (ops[0], ops[1]));
      break;

    case 3:
      emit_insn (GEN_FCN (icode) (ops[0], ops[1], ops[2]));
      break;

    case 4:
      emit_insn (GEN_FCN (icode) (ops[0], ops[1], ops[2], ops[3]));
      break;

    case 5:
      emit_insn (GEN_FCN (icode) (ops[0], ops[1], ops[2], ops[3], ops[4]));
      break;

    case 6:
      emit_insn (GEN_FCN (icode) (ops[0], ops[1], ops[2], ops[3], ops[4], ops[5]));
      break;

    default:
      gcc_unreachable ();
    }
  PulpBuiltinGenPostExtract(&ExtraArg, ops[0]);
  return target;
}

/* Implement TARGET_REMAPPED_BUILTIN */

static int
riscv_remapped_builtin(tree exp)

{
        tree fndecl = get_callee_fndecl (exp);
        enum built_in_function fcode = DECL_FUNCTION_CODE (fndecl);

        if (TARGET_MASK_OPEN_NATIVE) {
                fcode = (enum built_in_function) GetRemappedGompBuiltin(fcode, ARRAY_SIZE (riscv_builtins));
                if (fcode < ARRAY_SIZE (riscv_builtins)) return fcode;
        }
        return -1;
}

/* Implement TARGET_OMP_TARGET_DECL */

static tree
riscv_omp_target_decl(int t_omp_code, int *Base, int *Index)

{

        if (!TARGET_MASK_OPEN_NATIVE) return NULL;


        if (t_omp_code < 0 || t_omp_code >= NATIVE_GOMP_LAST) return NULL;
        if (Base) *Base = Native_GOMP_Builtins[t_omp_code].Base;
        if (Index) *Index = Native_GOMP_Builtins[t_omp_code].Index;
        return (Native_GOMP_Builtins[t_omp_code].TypeDescr);
}

/* Implement TARGET_EXPAND_BUILTIN.  */

/* Implement TARGET_EXPAND_BUILTIN.  */

rtx
riscv_expand_builtin (tree exp, rtx target, rtx subtarget ATTRIBUTE_UNUSED,
                     enum machine_mode mode ATTRIBUTE_UNUSED,
                     int ignore ATTRIBUTE_UNUSED)
{
  tree fndecl;
  unsigned int fcode, avail, fcode_remapped;
  const struct riscv_builtin_description *d;

  fndecl = TREE_OPERAND (CALL_EXPR_FN (exp), 0);
  fcode = DECL_FUNCTION_CODE (fndecl);
  if (fcode >= ARRAY_SIZE (riscv_builtins)) {
        fcode_remapped = GetRemappedGompBuiltin(fcode, ARRAY_SIZE (riscv_builtins));
        if (fcode_remapped < ARRAY_SIZE (riscv_builtins)) fcode = fcode_remapped;
  }

  gcc_assert (fcode < ARRAY_SIZE (riscv_builtins));
  d = &riscv_builtins[fcode];
  avail = d->avail ();
  gcc_assert (avail != 0);
  switch (d->builtin_type)
    {
    case RISCV_BUILTIN_DIRECT:
      return riscv_expand_builtin_direct (d, fcode, d->icode, target, exp, true);

    case RISCV_BUILTIN_DIRECT_NO_TARGET:
      return riscv_expand_builtin_direct (d, fcode, d->icode, target, exp, false);
    }
  gcc_unreachable ();
}

/* Implement TARGET_ATOMIC_ASSIGN_EXPAND_FENV.  */

void
riscv_atomic_assign_expand_fenv (tree *hold, tree *clear, tree *update)
{
  if (!TARGET_HARD_FLOAT)
    return;

  tree frflags = GET_BUILTIN_DECL (CODE_FOR_riscv_frflags);
  tree fsflags = GET_BUILTIN_DECL (CODE_FOR_riscv_fsflags);
  tree old_flags = create_tmp_var_raw (RISCV_ATYPE_USI);

  *hold = build2 (MODIFY_EXPR, RISCV_ATYPE_USI, old_flags,
		  build_call_expr (frflags, 0));
  *clear = build_call_expr (fsflags, 1, old_flags);
  *update = NULL_TREE;
}
