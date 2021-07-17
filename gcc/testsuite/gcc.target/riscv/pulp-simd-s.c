/* { dg-do run } */
/* { dg-options "-std=c11 -Os -march=rv32imc_xpulpvect_xpulpvectshufflepack -mabi=ilp32" } */

#include <stdlib.h>
#include <stdio.h>

typedef short v2hi  __attribute__ ((vector_size (4)));
typedef char  v4qi  __attribute__ ((vector_size (4)));

v2hi A = {100, 200};

v2hi init (unsigned short a, unsigned short b)
{
  return (v2hi) {a, b};
}
/* Move between registers */
v2hi move (v2hi a)
{
  return a;
}

/* Load from memory */
v2hi load ()
{
  return A;
}

/* Store to memory */
void store (v2hi a)
{
  A = a;
}

/* Add */
v2hi add (v2hi a, v2hi b)
{
  return a + b;
}

/* Subtract */
v2hi sub (v2hi a, v2hi b)
{
  return a - b;
}

/* Negate */
v2hi neg (v2hi a)
{
  return - a;
}

/* Multiply */
v2hi mul (v2hi a, v2hi b)
{
  return a * b;
}

/* avg */
v2hi avg (v2hi a, v2hi b)
{
  return (a + b) / 2;
}

/* Multiply and add */
v2hi madd (v2hi a, v2hi b, v2hi c)
{
  return a * b + c;
}

/* Multiply and subtract */
v2hi msub (v2hi a, v2hi b, v2hi c)
{
  return a * b - c;
}

/* Negate Multiply and add */
v2hi nmadd (v2hi a, v2hi b, v2hi c)
{
  return - (a * b + c);
}

/* Negate Multiply and subtract */
v2hi nmsub (v2hi a, v2hi b, v2hi c)
{
  return - (a * b - c);
}

/* Conditional Move */
v2hi cond_move1 (v2hi a, v2hi b, long i)
{
  if (i > 0)
    return a;
  else
    return b;
}

/* Conditional Move */
v2hi cond_move2 (v2hi a, v2hi b, int i)
{
  if (i > 0)
    return a;
  else
    return b;
}


int
main (void)
{
  v2hi a, b, c, d, e, f;
  unsigned short n1, n2;

  n1 = 34;
  n2 = 200;

  a = init (n1, n2);
  b = (v2hi) {34, 200};
  if (!(int)(a == b))
    abort ();

  a = (v2hi) {1, 2};
  b = (v2hi) {5, 6};
  b = move (a);

  if (!(int)(a == b))
    abort ();

  a = (v2hi) {1, 2};
  b = (v2hi) {5, 6};
  c = add (a, b);
  d = (v2hi) {6, 8};
  if (!(int)(c == d))
    abort ();

  a = (v2hi) {1, 12};
  b = (v2hi) {5, 6};
  c = sub (a, b);
  d = (v2hi) {-4, 6};
  if (!(int)(c == d))
    abort ();

  a = (v2hi) {1, 12};
  b = (v2hi) {5, 6};
  c = mul (a, b);
  d = (v2hi) {5, 72};
  if (!(int)(c == d))
    abort ();

  a = (v2hi) {1, 12};
  b = (v2hi) {5, 6};
  c = (v2hi) {5, 6};
  d = madd (a, b, c);
  e = (v2hi) {10, 78};
  if (!(int)(d == e))
    abort ();

  a = (v2hi) {1, 12};
  b = (v2hi) {5, 6};
  c = (v2hi) {5, 6};
  d = msub (a, b, c);
  e = (v2hi) {0, 66};
  if (!(int)(d == e))
    abort ();

  a = (v2hi) {1, 12};
  b = (v2hi) {5, 6};
  c = (v2hi) {5, 6};
  d = nmadd (a, b, c);
  e = (v2hi) {-10, -78};
  if (!(int)(d == e))
    abort ();

  a = (v2hi) {1, 12};
  b = (v2hi) {5, 6};
  c = (v2hi) {5, 6};
  d = nmsub (a, b, c);
  e = (v2hi) {0, -66};
  if (!(int)(d == e))
    abort ();

  a = (v2hi) {98, 12};
  b = neg (a);
  c = (v2hi) {-98, -12};
  if (!(int)(b == c))
    abort ();

  a = (v2hi) {1, 12};
  b = (v2hi) {5, 6};
  c = cond_move1 (a, b, 1000);
  if (!(int)(c == a))
    abort ();

  a = (v2hi) {1, 12};
  b = (v2hi) {5, 6};
  c = cond_move2 (a, b, -1000);
  if (!(int)(c == b))
    abort ();

  a = load();
  b = (v2hi) {100, 200};
  if (!(int)(a == b))
    abort ();

  a = (v2hi) {123, 321};
  store (a);
  b = load();
  if (!(int)(a == b))
    abort ();

  printf ("Test Passes\n");
  exit (0);

}
