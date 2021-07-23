/* { dg-do compile } */
/* { dg-options "-std=c11 -Os -march=rv32imc_xpulpv3 -mabi=ilp32" } */
void
pulp_builtin (void)
{
  volatile int var = __builtin_pulp_ClusterId();
}
/* { dg-final { scan-assembler "csrr" } } */

