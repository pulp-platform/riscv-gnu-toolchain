/* Definition of RISC-V target for GNU compiler.
   Copyright (C) 2016-2019 Free Software Foundation, Inc.
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

#ifndef GCC_RISCV_OPTS_H
#define GCC_RISCV_OPTS_H

enum riscv_abi_type {
  ABI_ILP32,
  ABI_ILP32E,
  ABI_ILP32F,
  ABI_ILP32D,
  ABI_LP64,
  ABI_LP64F,
  ABI_LP64D
};
extern enum riscv_abi_type riscv_abi;

enum riscv_code_model {
  CM_MEDLOW,
  CM_MEDANY,
  CM_PIC
};
extern enum riscv_code_model riscv_cmodel;

/* Keep this list in sync with define_attr "tune" in riscv.md.  */
enum riscv_microarchitecture_type {
  generic,
  sifive_7
};
extern enum riscv_microarchitecture_type riscv_microarchitecture;

enum riscv_align_data {
  riscv_align_data_type_xlen,
  riscv_align_data_type_natural
};

#ifndef _PULP_CHIP_OPTIONS_
#define _PULP_CHIP_OPTIONS_

enum Pulp_DP_Format_Type
{
	PULP_DP_FORMAT64=0,
	PULP_DP_FORMAT32=1
};

enum Pulp_Chip_Config_Type
{
	PULP_CHIP_CONFIG_ALL,
	PULP_CHIP_CONFIG_FC,
	PULP_CHIP_CONFIG_CLUSTER
};

enum Pulp_Processor_Type
{
  PULP_NONE,
  PULP_RISCV,
  PULP_SLIM,
  PULP_V0,
  PULP_V1,
  PULP_V2,
  PULP_V3,
/* __GAP8 Start */
  PULP_GAP8,
/* __GAP8 Stop */
  PULP_LAST
};

enum Pulp_Chip_Type
{
  PULP_CHIP_NONE,
  PULP_CHIP_HONEY,
  PULP_CHIP_PULPINO,
/* __GAP8 Start */
  PULP_CHIP_GAP8,
/* __GAP8 Stop */
  PULP_CHIP_LAST
};

struct Pulp_Target_Chip
{
        enum Pulp_Chip_Type chip;
        enum Pulp_Processor_Type processor;
        int Pulp_FC;
        int Pulp_PE;
        int Pulp_L2_Size;
        int Pulp_L1_Cluster_Size;
        int Pulp_L1_FC_Size;

};

/* Define _WITH_PULP_CHIP_INFO_FUNCT_ prior including this h file if you need private copy of functions
   to handle chip info and not only data structure definitions.
   This is to avoid warning on unused functions since this h file is included almost everywhere in gcc */

#ifdef _WITH_PULP_CHIP_INFO_FUNCT_

static struct Pulp_Target_Chip Pulp_Defined_Chips[PULP_CHIP_LAST] = {
/* None */      {PULP_CHIP_NONE,        PULP_RISCV,     0, 4, 1024*300, 1024*64, 0},
/* Honey */     {PULP_CHIP_HONEY,       PULP_V0,        0, 1, 1024*300, 1024*64, 0},
/* Pulpino */   {PULP_CHIP_PULPINO,     PULP_V1,        0, 1, 1024*300, 1024*64, 0},
/* __GAP8 Start */
/* Gap8 */      {PULP_CHIP_GAP8,        PULP_GAP8,      1, 8, 1024*512, 1024*64, 1024*32},
/* __GAP8 Stop */
};



static int Pulp_Check_Processor_Compatibility(enum Pulp_Processor_Type New_Proc, enum Pulp_Processor_Type With_Proc)

{
	static unsigned long long int Pulp_Compatible_Processors[PULP_LAST] = {
  		/* PULP_NONE */		(0),
  		/* PULP_RISCV */	((1<<PULP_RISCV)),
		/* PULP_SLIM */		((1<<PULP_RISCV) | (1<<PULP_SLIM)),
  		/* PULP_V0 */		((1<<PULP_RISCV) | (1<<PULP_V0)),
  		/* PULP_V1 */		((1<<PULP_RISCV) | (1<<PULP_V1)),
  		/* PULP_V2 */		((1<<PULP_RISCV) | (1<<PULP_V2)),
  		/* PULP_V3 */		((1<<PULP_RISCV) | (1<<PULP_V3)),
/* __GAP8 Start */
  		/* PULP_GAP8 */		((1<<PULP_RISCV) | (1<<PULP_V2) | (1<<PULP_GAP8)),
/* __GAP8 Stop */
	};

	return (Pulp_Compatible_Processors[New_Proc] & (1<<With_Proc));
}



// static struct Pulp_Target_Chip Pulp_Empty_Chip = {PULP_CHIP_NONE, PULP_NONE, -1, -1, -1, -1, -1};

static void UpdatePulpChip(struct Pulp_Target_Chip *Pulp_Chip, struct Pulp_Target_Chip *Sel)

{
        if (Pulp_Chip->chip == PULP_CHIP_NONE || Pulp_Chip->chip == Sel->chip) Pulp_Chip->chip = Sel->chip;
        if (Pulp_Chip->processor == PULP_NONE || Pulp_Chip->processor == Sel->processor) Pulp_Chip->processor = Sel->processor;
        if (Pulp_Chip->Pulp_FC == -1 || Pulp_Chip->Pulp_FC == Sel->Pulp_FC) Pulp_Chip->Pulp_FC = Sel->Pulp_FC;
        if (Pulp_Chip->Pulp_PE == -1 || Pulp_Chip->Pulp_PE == Sel->Pulp_PE) Pulp_Chip->Pulp_PE = Sel->Pulp_PE;
        if (Pulp_Chip->Pulp_L2_Size == -1 || Pulp_Chip->Pulp_L2_Size == Sel->Pulp_L2_Size) Pulp_Chip->Pulp_L2_Size = Sel->Pulp_L2_Size;
        if (Pulp_Chip->Pulp_L1_Cluster_Size == -1 || Pulp_Chip->Pulp_L1_Cluster_Size == Sel->Pulp_L1_Cluster_Size)
                Pulp_Chip->Pulp_L1_Cluster_Size = Sel->Pulp_L1_Cluster_Size;
        if (Pulp_Chip->Pulp_L1_FC_Size == -1 || Pulp_Chip->Pulp_L1_FC_Size == Sel->Pulp_L1_FC_Size)
                Pulp_Chip->Pulp_L1_FC_Size = Sel->Pulp_L1_FC_Size;
}

static enum Pulp_Chip_Type PulpDecodeChip(const char *Text)

{
        if      (strncmp (Text, "pulpino", 7) == 0) return PULP_CHIP_PULPINO;
        else if (strncmp (Text, "honey", 5) == 0) return PULP_CHIP_HONEY;
/* __GAP8 Start */
        else if (strncmp (Text, "gap8", 4) == 0) return PULP_CHIP_GAP8;
/* __GAP8 Stop */
        else if (strncmp (Text, "none", 4) == 0) return PULP_CHIP_NONE;
        else return PULP_CHIP_NONE;
}

static enum Pulp_Processor_Type PulpDecodeCpu(char *Text, int *Len)

{
	char *Dup = xstrdup (Text);
	int i;

	for (i = 0; Dup[i]; i++) Dup[i] = TOLOWER (Dup[i]);
        if      (strncmp (Dup, "none", 4) == 0)   { free(Dup); if (Len) *Len = 4; return PULP_NONE; }
        else if (strncmp (Dup, "riscv", 5) == 0)  { free(Dup); if (Len) *Len = 5; return PULP_RISCV; }
        else if (strncmp (Dup, "pulpv0", 6) == 0) { free(Dup); if (Len) *Len = 6; return PULP_V0; }
        else if (strncmp (Dup, "pulpv1", 6) == 0) { free(Dup); if (Len) *Len = 6; return PULP_V1; }
        else if (strncmp (Dup, "pulpv2", 6) == 0) { free(Dup); if (Len) *Len = 6; return PULP_V2; }
        else if (strncmp (Dup, "pulpv3", 6) == 0) { free(Dup); if (Len) *Len = 6; return PULP_V3; }
/* __GAP8 Start */
        else if (strncmp (Dup, "gap8", 4) == 0)   { free(Dup); if (Len) *Len = 4; return PULP_GAP8; }
/* __GAP8 Stop */
        else if (strncmp (Dup, "pulpslim", 8) == 0)   { free(Dup); if (Len) *Len = 8; return PULP_SLIM; }
        else                                      { free(Dup); if (Len) *Len = 0; return PULP_NONE; }
}

static int ExtractChipInfo(char *Text, struct Pulp_Target_Chip *ChipInfo)

{
        char *Pos;

	Pos = strstr(Text, "chip=");
        if (Pos != NULL ) ChipInfo->chip = PulpDecodeChip(Pos+5); else return 0;
        Pos = strstr(Text, "cpu=");
	if (Pos != NULL) ChipInfo->processor = PulpDecodeCpu(Pos+4, NULL); else return 0;
        Pos = strstr(Text, "pe=");
	if (Pos!= NULL)  ChipInfo->Pulp_PE = atoi(Pos+3); else return 0;
        Pos = strstr(Text, "fc=");
	if (Pos != NULL) ChipInfo->Pulp_FC = atoi(Pos+3); else return 0;
        Pos = strstr(Text, "l2=");
	if (Pos != NULL) ChipInfo->Pulp_L2_Size = atoi(Pos+3); else return 0;
        Pos = strstr(Text, "l1cl=");
	if (Pos != NULL) ChipInfo->Pulp_L1_Cluster_Size = atoi(Pos+5); else return 0;
        Pos = strstr(Text, "l1fc=");
	if (Pos != NULL) ChipInfo->Pulp_L1_FC_Size = atoi(Pos+5); else return 0;

        return 1;
}


static const char *PulpChipImage(enum Pulp_Chip_Type Which)

{
        switch (Which) {
                case PULP_CHIP_NONE: return "none";
                case PULP_CHIP_HONEY: return "honey";
                case PULP_CHIP_PULPINO: return "pulpino";
/* __GAP8 Start */
                case PULP_CHIP_GAP8: return "gap8";
/* __GAP8 Stop */
                default: return "Error";

        }
}

static const char *PulpProcessorImage(enum Pulp_Processor_Type Which)

{
        switch (Which) {
                case PULP_NONE: return "none";
                case PULP_RISCV: return "riscv";
                case PULP_V0: return "pulpv0";
                case PULP_V1: return "pulpv1";
                case PULP_V2: return "pulpv2";
                case PULP_V3: return "pulpv3";
/* __GAP8 Start */
                case PULP_GAP8: return "gap8";
/* __GAP8 Stop */
                case PULP_SLIM: return "pulpslim";
                default: return "Error";
        }
}

static char *PulpChipInfoImage(struct Pulp_Target_Chip *ChipInfo, char *Buf)

{
	sprintf(Buf, "chip=%s cpu=%s pe=%d fc=%d l2=%d l1cl=%d l1fc=%d",
                     PulpChipImage(ChipInfo->chip), PulpProcessorImage(ChipInfo->processor),
                     ChipInfo->Pulp_PE, ChipInfo->Pulp_FC, ChipInfo->Pulp_L2_Size,
                     ChipInfo->Pulp_L1_Cluster_Size, ChipInfo->Pulp_L1_FC_Size);
	return Buf;

}
#endif /* _WITH_PULP_CHIP_INFO_FUNCT_ */

#endif /* _PULP_CHIP_OPTIONS_ */

#endif /* ! GCC_RISCV_OPTS_H */
