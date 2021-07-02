/* { dg-do compile } */
/* { dg-options "-O -march=rv32ixbar_sabc_sxfoo -mabi=ilp32" } */
int foo()
{
}
/* { dg-warning "unknown extension" "" { target *-*-* } 0 } */
