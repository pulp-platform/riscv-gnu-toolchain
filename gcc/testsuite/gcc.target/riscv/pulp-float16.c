/* { dg-do compile } */
/* { dg-options "-march=rv32imfdc_xfhalf_xfhalfwithf_xfhalfwithd_xfalthalf_xfalthalfwithf" } */


#define CONV(TYPE, BASETYPE, BACK, FORTH)	\
  TYPE BACK (BASETYPE x)			\
  {						\
    return x;					\
  }						\
  						\
  BASETYPE FORTH (TYPE x)			\
  {						\
    return x;					\
  }


/* fp16 */

float16 x,y,z;

float16 fadd (float16 x, float16 y)
{
  return x + y;
}

float16 fsub (float16 x, float16 y)
{
  return x - y;
}

float16 fmul (float16 x, float16 y)
{
  return x * y;
}

float16 fdiv (float16 x, float16 y)
{
  return x / y;
}

float16 fmadd (float16 x, float16 y, float16 z)
{
  return (x * y) + z;
}

float16 fmsub (float16 x, float16 y, float16 z)
{
  return (x * y) - z;
}


float hftof (float16 x)
{
  return x;
}

float16 ftohf (float x)
{
  return x;
}

double hftod (float16 x)
{
  return x;
}

float16 dtohf (double x)
{
  return x;
}

/* int to float16 conversions */

CONV (int,           float16, hftosi, sitohf);
CONV (unsigned int,  float16, hftounsi, unsitohf);
CONV (long,          float16, hftodi, ditohf);
CONV (unsigned long, float16, hftoundi, unditohf);
/* TODO: needs libgcc support (?) */
/* CONV (long long,  float16, hftoti, titohf); */
/* CONV (unsigned long long, float16, hftounti, untitohf); */

/* fp16 alt */

float16alt a, b, c;

float16alt faddalt (float16alt a, float16alt b)
{
  return a + b;
}

float16alt fsubalt (float16alt a, float16alt b)
{
  return a - b;
}

float16alt fmulalt (float16alt a, float16alt b)
{
  return a * b;
}

float16alt fdivalt (float16alt a, float16alt b)
{
  return a / b;
}

float16alt fmaddalt (float16alt a, float16alt b, float16alt c)
{
  return (a * b) + c;
}

float16alt fmsubalt (float16alt a, float16alt b, float16alt c)
{
  return (a * b) - c;
}

/* int to float16 conversions */

CONV (int,           float16alt, ohftosi, sitoohf);
CONV (unsigned int,  float16alt, ohftounsi, unsitoohf);
CONV (long,          float16alt, ohftodi, ditoohf);
CONV (unsigned long, float16alt, ohftoundi, unditoohf);


/* fp16 / fp16 alt conversion */
/*
float16 hftoohf (float16alt a)
{
  return a;
}

float16alt ftoohf (float16 a)
{
  return a;
}
*/

int
main (void)
{
  return 0;
}
