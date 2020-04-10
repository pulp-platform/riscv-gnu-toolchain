/* { dg-do compile } */
/* { dg-options "-O -march=rv32isabc_xbar -mabi=ilp32" } */
int foo()
{
}
/* { dg-warning "unknown extension" "" { target *-*-* } 0 } */
