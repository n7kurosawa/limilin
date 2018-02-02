#ifndef RUNTIME_H_INCLUDED_
#define RUNTIME_H_INCLUDED_

#include <stdio.h>

typedef int t_int;

static inline int m_add(int x, int y) { return x + y; }
static inline int m_sub(int x, int y) { return x - y; }
static inline int m_mul(int x, int y) { return x * y; }
static inline int m_div(int x, int y) { return x / y; }
static inline int m_mod(int x, int y) { return x % y; }
static inline int m_neg(int x) { return -x; }
static inline int m_and(int x, int y) { return x & y; }
static inline int m_or(int x, int y) { return x | y; }
static inline int m_xor(int x, int y) { return x ^ y; }
static inline int m_not(int x) { return ~x; }
static inline int m_eq(int x, int y) { return x == y; }
static inline int m_neq(int x, int y) { return x != y; }
static inline int m_lt(int x, int y) { return x < y; }
static inline int m_gt(int x, int y) { return x > y; }
static inline int m_le(int x, int y) { return x <= y; }
static inline int m_ge(int x, int y) { return x >= y; }
static inline int m_putint(int x) { printf("%d",x); return 0;}
static inline int m_newline() { printf("\n"); return 0;}
static inline int m_putc(int x) { printf("%c",(char)x); return 0;}

#endif
