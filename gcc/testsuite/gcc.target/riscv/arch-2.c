/* { dg-do compile } */
/* { dg-options "-O -march=rv32ixabc_xfoo -mabi=ilp32" } */
int foo()
{
}
/* { dg-warning "unknown extension" "" { target *-*-* } 0 } */
