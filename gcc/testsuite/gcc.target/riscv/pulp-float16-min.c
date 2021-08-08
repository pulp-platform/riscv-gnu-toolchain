/* { dg-do compile } */
/* { dg-options "-march=rv32imfdc_xfhalf" } */

int
main (void)
{
  float16 a = 0.1f;
  float16 b = 0.2f;
  float16 c;
  c = a + b;
  return 0;
}
