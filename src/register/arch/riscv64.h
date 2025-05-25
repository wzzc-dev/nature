#ifndef NATURE_REGISTER_RISCV64_H
#define NATURE_REGISTER_RISCV64_H

#include "src/types.h"

// General-purpose registers (x0-x31)
extern reg_t *r_x0, *r_x1, *r_x2, *r_x3, *r_x4, *r_x5, *r_x6, *r_x7;
extern reg_t *r_x8, *r_x9, *r_x10, *r_x11, *r_x12, *r_x13, *r_x14, *r_x15;
extern reg_t *r_x16, *r_x17, *r_x18, *r_x19, *r_x20, *r_x21, *r_x22, *r_x23;
extern reg_t *r_x24, *r_x25, *r_x26, *r_x27, *r_x28, *r_x29, *r_x30, *r_x31;

// Special registers (aliases for general-purpose registers or dedicated)
extern reg_t *r_zero; // Hardwired zero (alias for x0)
extern reg_t *r_ra;   // Return address (alias for x1)
extern reg_t *r_sp;   // Stack pointer (alias for x2)
extern reg_t *r_gp;   // Global pointer (alias for x3)
extern reg_t *r_tp;   // Thread pointer (alias for x4)
extern reg_t *r_fp;   // Frame pointer (alias for x8/s0)
extern reg_t *r_pc;   // Program counter (not a GPR, but essential)

// Floating-point registers (f0-f31 for double-precision, also used for single-precision)
extern reg_t *r_f0, *r_f1, *r_f2, *r_f3, *r_f4, *r_f5, *r_f6, *r_f7;
extern reg_t *r_f8, *r_f9, *r_f10, *r_f11, *r_f12, *r_f13, *r_f14, *r_f15;
extern reg_t *r_f16, *r_f17, *r_f18, *r_f19, *r_f20, *r_f21, *r_f22, *r_f23;
extern reg_t *r_f24, *r_f25, *r_f26, *r_f27, *r_f28, *r_f29, *r_f30, *r_f31;

// RISC-V specific counts and sizes
// Integer registers: x5-x7 (t0-t2), x9 (s1), x10-x17 (a0-a7), x18-x27 (s2-s11), x28-x31 (t3-t6)
// Count: 3 (t0-t2) + 1 (s1) + 8 (a0-a7) + 10 (s2-s11) + 4 (t3-t6) = 26 allocatable integer registers.
// (x0=zero, x1=ra, x2=sp, x3=gp, x4=tp, x8=s0/fp are special or have fixed roles)
#define RISCV64_ALLOC_INT_REG_COUNT 26

// Floating-point registers: f0-f7 (ft0-ft7), f9 (fs1), f10-f17 (fa0-fa7), f18-f27 (fs2-fs11), f28-f31 (ft8-ft11)
// Count: 8 (ft0-ft7) + 1 (fs1) + 8 (fa0-fa7) + 10 (fs2-fs11) + 4 (ft8-ft11) = 31 allocatable float registers.
// (f8=fs0 is callee-saved but often used like other saved registers)
#define RISCV64_ALLOC_FLOAT_REG_COUNT 31

#define RISCV64_ALLOC_REG_COUNT (RISCV64_ALLOC_INT_REG_COUNT + RISCV64_ALLOC_FLOAT_REG_COUNT)
#define RISCV64_STACK_ALIGN_SIZE 16

void riscv64_reg_init();

#endif //NATURE_REGISTER_RISCV64_H