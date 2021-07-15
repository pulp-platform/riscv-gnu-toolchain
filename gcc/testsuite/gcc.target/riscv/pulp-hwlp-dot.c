/* { dg-do compile } */
/* { dg-options "-std=c11 -Os -march=rv32imc_xpulphwloop -mabi=ilp32" } */
#include <stdlib.h>

#define LEN 128

int a[LEN];
int b[LEN];
int c[LEN];

int __attribute__ ((noinline))
dot (const int *vec_a, const int *vec_b, int *vec_c, int len)
{
  int i;
  for (i = 0; i < len; i++)
    vec_c[i] = vec_a[i] * vec_b[i];
}

int
main (void)
{
  for (int i = 0; i < LEN; i++)
    {
      a[i] = i;
      b[i] = i;
      c[i] = 0;
    }
  dot (a, b, c, LEN);

  exit (0);
}

/* { dg-final { scan-assembler "lp.setup" } } */
