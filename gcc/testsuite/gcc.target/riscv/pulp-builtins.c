/* { dg-do compile } */
/* { dg-options "-march=rv32imcxpulpv3"} */

int main(void)
{
#define SR_MTVEC 0x305

  int somevar = 0;
  int othervar = 22;
  volatile int arg0 = 1;
  volatile int arg1 = 1;
  void* ptr = &somevar;

  int ret0, ret1, ret2, ret3, ret5, ret6, ret11, ret12;
  int tmp0, tmp1, tmp2;

  /* somehow the builtin constraint want us to directly enter 22 withouth var */
  ret0 = __builtin_pulp_event_unit_read(ptr, 22);
  ret1 = __builtin_pulp_OffsetedRead(ptr, arg0);
  ret2 = __builtin_pulp_OffsetedReadHalf(ptr, arg0);
  ret3 = __builtin_pulp_OffsetedReadByte(ptr, arg0);

  /* omp not supported */
  /* int ret4 = __builtin_pulp_OffsetedReadOMP(ptr, arg0); */

  /* It is better you pretend these don't exists. They rely on hardcoded
     addresses of PULP. Insanity. */

  /* segfaults ? */
  /* int ret4 = __builtin_pulp_CoreCount(); */
  ret5 = __builtin_pulp_CoreId();
  ret6 = __builtin_pulp_ClusterId();
  /* expects spr to be immedate in 0..4091 ? */
  /* int ret7 = __builtin_pulp_IsFc(); */
  /* expects spr to be immedate in 0..4091 ? */
  /* int ret8 = __builtin_pulp_HasFc(); */

  /* type mismatch ? */
  /* int ret9 = __builtin_pulp_read_base_off(ptr, arg1); */
  __builtin_pulp_write_base_off(ptr, arg1, arg1);


  ret11 = __builtin_pulp_spr_read_vol(SR_MTVEC);
  ret12 = __builtin_pulp_spr_read(SR_MTVEC);

  __builtin_pulp_spr_write(SR_MTVEC, 0xdeadbeef);
  __builtin_pulp_spr_bit_set(SR_MTVEC, 0xdeadbeef);
  __builtin_pulp_spr_bit_clr(SR_MTVEC, 0xdeadbeef);

  tmp0 = __builtin_pulp_read_then_spr_write(SR_MTVEC, SR_MTVEC);
  tmp1 = __builtin_pulp_read_then_spr_bit_set(SR_MTVEC, SR_MTVEC);
  tmp2 = __builtin_pulp_read_then_spr_bit_clr(SR_MTVEC, SR_MTVEC);

  /* type mismatch ? */
  /* int tmp3 = __builtin_pulp_read_base_off_v(arg0,arg1); */
  __builtin_pulp_write_base_off_v(arg0, arg1, arg1);

  /* omp not supported */
  /* __builtin_pulp_GOMP_barrier */
  /* __builtin_pulp_GOMP_critical_start */
  /* __builtin_pulp_GOMP_critical_end */
  return 0;
}
