/* { dg-do compile } */
/* { dg-options "-std=c11 -Os -march=rv32imc_xpulphwloop -mabi=ilp32" } */
#include <stdlib.h>

#define M 50
#define N 50
#define K 51

int mat_a[M * K];
int mat_b[K * N];
int mat_c[M * N];

int
matmul (const int *A, const int *B, int *C, int M1, int N1, int K1)
{
  int m, n, k;
  for (m = 0; m < M1; m++)
    {
      for (n = 0; n < N1; n++)
        {
          for (k = 0; k < K1; k++)
            {
              C[M1 * m + n] += A[M1 * m + k] * B[N1 * k + n];
            }
        }
    }
}

int
main (void)
{
  for (int i = 0; i < M * K; i++)
    mat_a[i] = 1;
  for (int i = 0; i < K * N; i++)
    mat_b[i] = 1;
  for (int i = 0; i < M * N; i++)
    mat_c[i] = 0;

  matmul (mat_a, mat_b, mat_c, M, N, K);

  exit (0);
}

/* { dg-final { scan-assembler "lp.setup" } } */
/* TODO: this test is not finished. Nested hwloops don't work */
