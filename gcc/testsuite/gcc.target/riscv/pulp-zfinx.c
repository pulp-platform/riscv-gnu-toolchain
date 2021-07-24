/* { dg-do compile } */
/* { dg-options "-march=rv32i_zfinx -mabi=ilp32" } */
void
foo (void)
{
  volatile float a, b, c;
  a = 0.1f;
  b = 0.5f;
  c = a * b;
}
/* { dg-final { scan-assembler-not "flw" } } */
/* { dg-final { scan-assembler-not "fsw" } } */
