#include "riscv64.h"
#include "utils/type.h"
#include "utils/slice.h"
#include "src/register/register.h"
#include <stdio.h>

// General-purpose registers (x0-x31)
reg_t *r_x0, *r_x1, *r_x2, *r_x3, *r_x4, *r_x5, *r_x6, *r_x7;
reg_t *r_x8, *r_x9, *r_x10, *r_x11, *r_x12, *r_x13, *r_x14, *r_x15;
reg_t *r_x16, *r_x17, *r_x18, *r_x19, *r_x20, *r_x21, *r_x22, *r_x23;
reg_t *r_x24, *r_x25, *r_x26, *r_x27, *r_x28, *r_x29, *r_x30, *r_x31;

// Special registers (aliases for general-purpose registers or dedicated)
reg_t *r_zero; // Hardwired zero (alias for x0)
reg_t *r_ra;   // Return address (alias for x1)
reg_t *r_sp;   // Stack pointer (alias for x2)
reg_t *r_gp;   // Global pointer (alias for x3)
reg_t *r_tp;   // Thread pointer (alias for x4)
reg_t *r_fp;   // Frame pointer (alias for x8/s0)
reg_t *r_pc;   // Program counter (not a GPR, but essential)

// Floating-point registers (f0-f31 for double-precision, also used for single-precision)
reg_t *r_f0, *r_f1, *r_f2, *r_f3, *r_f4, *r_f5, *r_f6, *r_f7;
reg_t *r_f8, *r_f9, *r_f10, *r_f11, *r_f12, *r_f13, *r_f14, *r_f15;
reg_t *r_f16, *r_f17, *r_f18, *r_f19, *r_f20, *r_f21, *r_f22, *r_f23;
reg_t *r_f24, *r_f25, *r_f26, *r_f27, *r_f28, *r_f29, *r_f30, *r_f31;

void riscv64_reg_init() {
    // General-purpose registers initialization
    // ABI Name | Register | Description | Saver | Allocatable ID (0 if not allocatable or special)
    r_x0 = reg_new("r_x0", 0, 0, QWORD, 0); r_zero = r_x0; // zero: Hard-wired zero
    r_x1 = reg_new("r_x1", 1, 0, QWORD, 0); r_ra = r_x1;   // ra: Return address (Caller)
    r_x2 = reg_new("r_x2", 2, 0, QWORD, 0); r_sp = r_x2;   // sp: Stack pointer (Callee)
    r_x3 = reg_new("r_x3", 3, 0, QWORD, 0); r_gp = r_x3;   // gp: Global pointer
    r_x4 = reg_new("r_x4", 4, 0, QWORD, 0); r_tp = r_x4;   // tp: Thread pointer

    r_x5 = reg_new("r_x5", 5, LIR_FLAG_ALLOC_INT, QWORD, 1);  // t0: Temporary (Caller)
    r_x6 = reg_new("r_x6", 6, LIR_FLAG_ALLOC_INT, QWORD, 2);  // t1: Temporary (Caller)
    r_x7 = reg_new("r_x7", 7, LIR_FLAG_ALLOC_INT, QWORD, 3);  // t2: Temporary (Caller)

    r_x8 = reg_new("r_x8", 8, 0, QWORD, 0); r_fp = r_x8;   // s0/fp: Saved register/frame pointer (Callee)

    r_x9 = reg_new("r_x9", 9, LIR_FLAG_ALLOC_INT, QWORD, 4);  // s1: Saved register (Callee)

    r_x10 = reg_new("r_x10", 10, LIR_FLAG_ALLOC_INT, QWORD, 5); // a0: Function argument/return value (Caller)
    r_x11 = reg_new("r_x11", 11, LIR_FLAG_ALLOC_INT, QWORD, 6); // a1: Function argument/return value (Caller)
    r_x12 = reg_new("r_x12", 12, LIR_FLAG_ALLOC_INT, QWORD, 7); // a2: Function argument (Caller)
    r_x13 = reg_new("r_x13", 13, LIR_FLAG_ALLOC_INT, QWORD, 8); // a3: Function argument (Caller)
    r_x14 = reg_new("r_x14", 14, LIR_FLAG_ALLOC_INT, QWORD, 9); // a4: Function argument (Caller)
    r_x15 = reg_new("r_x15", 15, LIR_FLAG_ALLOC_INT, QWORD, 10); // a5: Function argument (Caller)
    r_x16 = reg_new("r_x16", 16, LIR_FLAG_ALLOC_INT, QWORD, 11); // a6: Function argument (Caller)
    r_x17 = reg_new("r_x17", 17, LIR_FLAG_ALLOC_INT, QWORD, 12); // a7: Function argument (Caller)

    r_x18 = reg_new("r_x18", 18, LIR_FLAG_ALLOC_INT, QWORD, 13); // s2: Saved register (Callee)
    r_x19 = reg_new("r_x19", 19, LIR_FLAG_ALLOC_INT, QWORD, 14); // s3: Saved register (Callee)
    r_x20 = reg_new("r_x20", 20, LIR_FLAG_ALLOC_INT, QWORD, 15); // s4: Saved register (Callee)
    r_x21 = reg_new("r_x21", 21, LIR_FLAG_ALLOC_INT, QWORD, 16); // s5: Saved register (Callee)
    r_x22 = reg_new("r_x22", 22, LIR_FLAG_ALLOC_INT, QWORD, 17); // s6: Saved register (Callee)
    r_x23 = reg_new("r_x23", 23, LIR_FLAG_ALLOC_INT, QWORD, 18); // s7: Saved register (Callee)
    r_x24 = reg_new("r_x24", 24, LIR_FLAG_ALLOC_INT, QWORD, 19); // s8: Saved register (Callee)
    r_x25 = reg_new("r_x25", 25, LIR_FLAG_ALLOC_INT, QWORD, 20); // s9: Saved register (Callee)
    r_x26 = reg_new("r_x26", 26, LIR_FLAG_ALLOC_INT, QWORD, 21); // s10: Saved register (Callee)
    r_x27 = reg_new("r_x27", 27, LIR_FLAG_ALLOC_INT, QWORD, 22); // s11: Saved register (Callee)

    r_x28 = reg_new("r_x28", 28, LIR_FLAG_ALLOC_INT, QWORD, 23); // t3: Temporary (Caller)
    r_x29 = reg_new("r_x29", 29, LIR_FLAG_ALLOC_INT, QWORD, 24); // t4: Temporary (Caller)
    r_x30 = reg_new("r_x30", 30, LIR_FLAG_ALLOC_INT, QWORD, 25); // t5: Temporary (Caller)
    r_x31 = reg_new("r_x31", 31, LIR_FLAG_ALLOC_INT, QWORD, 26); // t6: Temporary (Caller)

    // Program Counter (not a GPR, but essential for control flow)
    r_pc = reg_new("r_pc", -1, 0, QWORD, 0); // Using -1 as a conventional ID for PC

    // Floating-point registers initialization (f0-f31)
    // ABI Name | Register | Description | Saver | Allocatable ID (offset by RISCV64_ALLOC_INT_REG_COUNT)
    int f_offset = RISCV64_ALLOC_INT_REG_COUNT;

    r_f0  = reg_new("r_f0",  0,  LIR_FLAG_ALLOC_FLOAT, QWORD, f_offset + 1);  // ft0: FP temporary (Caller)
    r_f1  = reg_new("r_f1",  1,  LIR_FLAG_ALLOC_FLOAT, QWORD, f_offset + 2);  // ft1: FP temporary (Caller)
    r_f2  = reg_new("r_f2",  2,  LIR_FLAG_ALLOC_FLOAT, QWORD, f_offset + 3);  // ft2: FP temporary (Caller)
    r_f3  = reg_new("r_f3",  3,  LIR_FLAG_ALLOC_FLOAT, QWORD, f_offset + 4);  // ft3: FP temporary (Caller)
    r_f4  = reg_new("r_f4",  4,  LIR_FLAG_ALLOC_FLOAT, QWORD, f_offset + 5);  // ft4: FP temporary (Caller)
    r_f5  = reg_new("r_f5",  5,  LIR_FLAG_ALLOC_FLOAT, QWORD, f_offset + 6);  // ft5: FP temporary (Caller)
    r_f6  = reg_new("r_f6",  6,  LIR_FLAG_ALLOC_FLOAT, QWORD, f_offset + 7);  // ft6: FP temporary (Caller)
    r_f7  = reg_new("r_f7",  7,  LIR_FLAG_ALLOC_FLOAT, QWORD, f_offset + 8);  // ft7: FP temporary (Caller)

    r_f8  = reg_new("r_f8",  8,  LIR_FLAG_ALLOC_FLOAT, QWORD, f_offset + 9);  // fs0: FP saved register (Callee)
    r_f9  = reg_new("r_f9",  9,  LIR_FLAG_ALLOC_FLOAT, QWORD, f_offset + 10); // fs1: FP saved register (Callee)

    r_f10 = reg_new("r_f10", 10, LIR_FLAG_ALLOC_FLOAT, QWORD, f_offset + 11); // fa0: FP argument/return value (Caller)
    r_f11 = reg_new("r_f11", 11, LIR_FLAG_ALLOC_FLOAT, QWORD, f_offset + 12); // fa1: FP argument/return value (Caller)
    r_f12 = reg_new("r_f12", 12, LIR_FLAG_ALLOC_FLOAT, QWORD, f_offset + 13); // fa2: FP argument (Caller)
    r_f13 = reg_new("r_f13", 13, LIR_FLAG_ALLOC_FLOAT, QWORD, f_offset + 14); // fa3: FP argument (Caller)
    r_f14 = reg_new("r_f14", 14, LIR_FLAG_ALLOC_FLOAT, QWORD, f_offset + 15); // fa4: FP argument (Caller)
    r_f15 = reg_new("r_f15", 15, LIR_FLAG_ALLOC_FLOAT, QWORD, f_offset + 16); // fa5: FP argument (Caller)
    r_f16 = reg_new("r_f16", 16, LIR_FLAG_ALLOC_FLOAT, QWORD, f_offset + 17); // fa6: FP argument (Caller)
    r_f17 = reg_new("r_f17", 17, LIR_FLAG_ALLOC_FLOAT, QWORD, f_offset + 18); // fa7: FP argument (Caller)

    r_f18 = reg_new("r_f18", 18, LIR_FLAG_ALLOC_FLOAT, QWORD, f_offset + 19); // fs2: FP saved register (Callee)
    r_f19 = reg_new("r_f19", 19, LIR_FLAG_ALLOC_FLOAT, QWORD, f_offset + 20); // fs3: FP saved register (Callee)
    r_f20 = reg_new("r_f20", 20, LIR_FLAG_ALLOC_FLOAT, QWORD, f_offset + 21); // fs4: FP saved register (Callee)
    r_f21 = reg_new("r_f21", 21, LIR_FLAG_ALLOC_FLOAT, QWORD, f_offset + 22); // fs5: FP saved register (Callee)
    r_f22 = reg_new("r_f22", 22, LIR_FLAG_ALLOC_FLOAT, QWORD, f_offset + 23); // fs6: FP saved register (Callee)
    r_f23 = reg_new("r_f23", 23, LIR_FLAG_ALLOC_FLOAT, QWORD, f_offset + 24); // fs7: FP saved register (Callee)
    r_f24 = reg_new("r_f24", 24, LIR_FLAG_ALLOC_FLOAT, QWORD, f_offset + 25); // fs8: FP saved register (Callee)
    r_f25 = reg_new("r_f25", 25, LIR_FLAG_ALLOC_FLOAT, QWORD, f_offset + 26); // fs9: FP saved register (Callee)
    r_f26 = reg_new("r_f26", 26, LIR_FLAG_ALLOC_FLOAT, QWORD, f_offset + 27); // fs10: FP saved register (Callee)
    r_f27 = reg_new("r_f27", 27, LIR_FLAG_ALLOC_FLOAT, QWORD, f_offset + 28); // fs11: FP saved register (Callee)

    r_f28 = reg_new("r_f28", 28, LIR_FLAG_ALLOC_FLOAT, QWORD, f_offset + 29); // ft8: FP temporary (Caller)
    r_f29 = reg_new("r_f29", 29, LIR_FLAG_ALLOC_FLOAT, QWORD, f_offset + 30); // ft9: FP temporary (Caller)
    r_f30 = reg_new("r_f30", 30, LIR_FLAG_ALLOC_FLOAT, QWORD, f_offset + 31); // ft10: FP temporary (Caller)
    r_f31 = reg_new("r_f31", 31, LIR_FLAG_ALLOC_FLOAT, QWORD, f_offset + 32); // ft11: FP temporary (Caller) - Note: RISCV64_ALLOC_FLOAT_REG_COUNT is 31, so this ID might be f_offset + 31 if we count from 0. Let's assume allocatable IDs are 1-based for consistency with the define.
}