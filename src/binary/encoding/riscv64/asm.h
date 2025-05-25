#ifndef NATURE_RISCV64_ASM_H
#define NATURE_RISCV64_ASM_H

#include "src/register/arch/riscv64.h"
#include "src/types.h"

#define RO_INDIRECT(_reg, _offset, _size) ({                               \
    riscv64_asm_operand_t *_indirect_operand = NEW(riscv64_asm_operand_t); \
    _indirect_operand->type = RISCV64_ASM_OPERAND_INDIRECT;                \
    _indirect_operand->indirect.reg = _reg;                                \
    _indirect_operand->indirect.offset = _offset;                          \
    _indirect_operand;                                                     \
})

#define RO_IMM(_imm) ({                                               \
    riscv64_asm_operand_t *_imm_operand = NEW(riscv64_asm_operand_t); \
    _imm_operand->type = RISCV64_ASM_OPERAND_IMMEDIATE;               \
    _imm_operand->immediate = _imm;                                   \
    _imm_operand;                                                     \
})

#define RO_SYM(_name, _is_local, _offset, _reloc_type) ({         \
    riscv64_asm_operand_t *_operand = NEW(riscv64_asm_operand_t); \
    _operand->type = RISCV64_ASM_OPERAND_SYMBOL;                  \
    _operand->symbol.name = _name;                                \
    _operand->symbol.is_local = _is_local;                        \
    _operand->symbol.offset = _offset;                            \
    _operand;                                                     \
})

#define RO_REG(_reg) ({                                               \
    riscv64_asm_operand_t *_reg_operand = NEW(riscv64_asm_operand_t); \
    if (FLAG(LIR_FLAG_ALLOC_FLOAT) & _reg->flag) {                    \
        _reg_operand->type = RISCV64_ASM_OPERAND_FREG;                \
    } else {                                                          \
        _reg_operand->type = RISCV64_ASM_OPERAND_REG;                 \
    }                                                                 \
    _reg_operand->reg = *_reg;                                        \
    _reg_operand;                                                     \
})

#define RISCV64_INST(_raw_opcode, ...) ({                     \
    riscv64_asm_inst_t *_inst = NEW(riscv64_asm_inst_t);      \
    _inst->op_id = op->id;                                    \
    _inst->line = op->line;                                   \
    _inst->column = op->column;                               \
    _inst->raw_opcode = _raw_opcode;                          \
                                                              \
    riscv64_asm_operand_t *_temp_operands[4] = {__VA_ARGS__}; \
                                                              \
    for (int _i = 0; _i < 4; ++_i) {                          \
        if (_temp_operands[_i] != NULL) {                     \
            _inst->operands[_i] = _temp_operands[_i];         \
            _inst->count++;                                   \
        }                                                     \
    }                                                         \
    _inst;                                                    \
})

typedef enum {
    R_NOOP,
    R_MV,
    R_LI,
    R_LA,
    R_ADD,
    R_ADDW,
    R_ADDI,
    R_ADDIW,
    R_SUB,
    R_SUBW,
    R_MUL,
    R_MULW,
    R_DIV,
    R_DIVU,
    R_DIVW,
    R_DIVUW,
    R_REM,
    R_REMU,
    R_REMW,
    R_REMUW,
    R_AND,
    R_ANDI,
    R_OR,
    R_ORI,
    R_XOR,
    R_XORI,
    R_NEG,
    R_NOT,
    R_SEXT_B,
    R_SEXT_H,
    R_SEXT_W,
    R_ZEXT_B,
    R_ZEXT_H,
    R_ZEXT_W,
    R_SLL,
    R_SLLW,
    R_SLLI,
    R_SLLIW,
    R_SRL,
    R_SRLW,
    R_SRLI,
    R_SRLIW,
    R_SRA,
    R_SRAW,
    R_SRAI,
    R_SRAIW,
    R_LB,
    R_LH,
    R_LW,
    R_LD,
    R_LBU,
    R_LHU,
    R_LWU,
    R_SB,
    R_SH,
    R_SW,
    R_SD,
    R_SLT,
    R_SLTU,
    R_SLTI,
    R_SLTIU,
    R_SEQZ,
    R_SNEZ,
    R_SLTZ,
    R_SGTZ,
    R_J,
    R_JR,
    R_JALR,
    R_BEQ,
    R_BNE,
    R_BLT,
    R_BGE,
    R_BLTU,
    R_BGEU,
    R_CALL,
    R_RET,
    R_ECALL,

    R_FADD_D,
    R_FSUB_D,
    R_FMUL_D,
    R_FDIV_D,
    R_FADD_S,
    R_FSUB_S,
    R_FMUL_S,
    R_FDIV_S,
    R_FSQRT_D,
    R_FSQRT_S,
    R_FSGNJ_D,
    R_FSGNJN_D,
    R_FSGNJX_D,
    R_FSGNJ_S,
    R_FSGNJN_S,
    R_FSGNJX_S,
    R_FMV_D,
    R_FNEG_D,
    R_FMV_S,
    R_FNEG_S,
    R_FMV_X_D,
    R_FMV_X_W,
    R_FEQ_D,
    R_FLT_D,
    R_FLE_D,
    R_FEQ_S,
    R_FLT_S,
    R_FLE_S,
    R_FLD,
    R_FLW,
    R_FSD,
    R_FSW,

    R_FCVT_D_W,
    R_FCVT_D_WU,
    R_FCVT_D_L,
    R_FCVT_D_LU,
    R_FCVT_W_D,
    R_FCVT_WU_D,
    R_FCVT_L_D,
    R_FCVT_LU_D,
    R_FCVT_S_W,
    R_FCVT_S_WU,
    R_FCVT_S_L,
    R_FCVT_S_LU,
    R_FCVT_W_S,
    R_FCVT_WU_S,
    R_FCVT_L_S,
    R_FCVT_LU_S,
    R_FCVT_D_S,
    R_FCVT_S_D,
} riscv64_asm_raw_opcode_t;

typedef enum {
    RISCV64_NOROUND = -1,
    RISCV64_RNE, // Round to Nearest, ties to Even
    RISCV64_RTZ, // Round towards Zero
    RISCV64_RDN, // Round Down (towards -Inf)
    RISCV64_RUP, // Round Up (towards +Inf)
    RISCV64_RMM, // Round to Nearest, ties to Max Magnitude
} riscv64_round_mode_t;

#define RISCV64_R64 (1 << 0)
#define RISCV64_F64 (1 << 1)
#define RISCV64_IMM (1 << 2)
#define RISCV64_IND (1 << 3)
#define RISCV64_SYM (1 << 4)
#define RISCV64_RND (1 << 10)

typedef enum {
    RISCV64_ASM_OPERAND_NOOPERAND,
    RISCV64_ASM_OPERAND_REG, // reg
    RISCV64_ASM_OPERAND_IMMEDIATE, // 1234
    RISCV64_ASM_OPERAND_DIRECT, // foobar + 345
    RISCV64_ASM_OPERAND_INDIRECT, // ofs(reg)
    RISCV64_ASM_OPERAND_FREG, // freg
    RISCV64_ASM_OPERAND_ROUNDMODE, // rm

    RISCV64_ASM_OPERAND_SYMBOL, // label 或者 symbol 符号，汇编器基于此进行 offset 计算。编译二进制时通常使用占位符号占用
} riscv64_asm_operand_type_t;

static char *riscv64_raw_op_names[] = {
        "mv",
        "li",
        "la",
        "add",
        "addw",
        "addi",
        "addiw",
        "sub",
        "subw",
        "mul",
        "mulw",
        "div",
        "divu",
        "divw",
        "divuw",
        "rem",
        "remu",
        "remw",
        "remuw",
        "and",
        "andi",
        "or",
        "ori",
        "xor",
        "xori",
        "neg",
        "not",
        "sext.b",
        "sext.h",
        "sext.w",
        "zext.b",
        "zext.h",
        "zext.w",
        "sll",
        "sllw",
        "slli",
        "slliw",
        "srl",
        "srlw",
        "srli",
        "srliw",
        "sra",
        "sraw",
        "srai",
        "sraiw",
        "lb",
        "lh",
        "lw",
        "ld",
        "lbu",
        "lhu",
        "lwu",
        "sb",
        "sh",
        "sw",
        "sd",
        "slt",
        "sltu",
        "slti",
        "sltiu",
        "seqz",
        "snez",
        "sltz",
        "sgtz",
        "j",
        "jr",
        "jalr",
        "beq",
        "bne",
        "blt",
        "bge",
        "bltu",
        "bgeu",
        "call",
        "ret",
        "ecall",

        "fadd.d",
        "fsub.d",
        "fmul.d",
        "fdiv.d",
        "fadd.s",
        "fsub.s",
        "fmul.s",
        "fdiv.s",
        "fsqrt.d",
        "fsqrt.s",
        "fsgnj.d",
        "fsgnjn.d",
        "fsgnjx.d",
        "fsgnj.s",
        "fsgnjn.s",
        "fsgnjx.s",
        "fmv.d",
        "fneg.d",
        "fmv.s",
        "fneg.s",
        "fmv.x.d",
        "fmv.x.w",
        "feq.d",
        "flt.d",
        "fle.d",
        "feq.s",
        "flt.s",
        "fle.s",
        "fld",
        "flw",
        "fsd",
        "fsw",
        "fcvt.d.w",
        "fcvt.d.wu",
        "fcvt.d.l",
        "fcvt.d.lu",
        "fcvt.w.d",
        "fcvt.wu.d",
        "fcvt.l.d",
        "fcvt.lu.d",
        "fcvt.s.w",
        "fcvt.s.wu",
        "fcvt.s.l",
        "fcvt.s.lu",
        "fcvt.w.s",
        "fcvt.wu.s",
        "fcvt.l.s",
        "fcvt.lu.s",
        "fcvt.d.s",
        "fcvt.s.d",
        NULL,
};

typedef enum {
    O_NOOP,
    O_MV,
    O_LI,
    O_LA,
    O_ADD,
    O_ADDW,
    O_ADDI,
    O_ADDIW,
    O_SUB,
    O_SUBW,
    O_MUL,
    O_MULW,
    O_DIV,
    O_DIVU,
    O_DIVW,
    O_DIVUW,
    O_REM,
    O_REMU,
    O_REMW,
    O_REMUW,
    O_AND,
    O_ANDI,
    O_OR,
    O_ORI,
    O_XOR,
    O_XORI,
    O_NEG,
    O_NOT,
    O_SEXT_B,
    O_SEXT_H,
    O_SEXT_W,
    O_ZEXT_B,
    O_ZEXT_H,
    O_ZEXT_W,
    O_SLL,
    O_SLLW,
    O_SLLI,
    O_SLLIW,
    O_SRL,
    O_SRLW,
    O_SRLI,
    O_SRLIW,
    O_SRA,
    O_SRAW,
    O_SRAI,
    O_SRAIW,
    O_LB,
    O_LH,
    O_LW,
    O_LD,
    O_LBU,
    O_LHU,
    O_LWU,
    O_SB,
    O_SH,
    O_SW,
    O_SD,
    O_SLT,
    O_SLTU,
    O_SLTI,
    O_SLTIU,
    O_SEQZ,
    O_SNEZ,
    O_SLTZ,
    O_SGTZ,
    O_J,
    O_JR,
    O_JALR,
    O_BEQ,
    O_BNE,
    O_BLT,
    O_BGE,
    O_BLTU,
    O_BGEU,
    O_CALL,
    O_RET,
    O_ECALL,

    O_FADD_D,
    O_FSUB_D,
    O_FMUL_D,
    O_FDIV_D,
    O_FADD_S,
    O_FSUB_S,
    O_FMUL_S,
    O_FDIV_S,
    O_FSQRT_D,
    O_FSQRT_S,
    O_FSGNJ_D,
    O_FSGNJN_D,
    O_FSGNJX_D,
    O_FSGNJ_S,
    O_FSGNJN_S,
    O_FSGNJX_S,
    O_FMV_D,
    O_FNEG_D,
    O_FMV_S,
    O_FNEG_S,
    O_FMV_X_D,
    O_FMV_X_W,
    O_FEQ_D,
    O_FLT_D,
    O_FLE_D,
    O_FEQ_S,
    O_FLT_S,
    O_FLE_S,
    O_FLD,
    O_FLW,
    O_FSD,
    O_FSW,

    O_FCVT_D_W,
    O_FCVT_D_WU,
    O_FCVT_D_L,
    O_FCVT_D_LU,
    O_FCVT_W_D,
    O_FCVT_WU_D,
    O_FCVT_L_D,
    O_FCVT_LU_D,
    O_FCVT_S_W,
    O_FCVT_S_WU,
    O_FCVT_S_L,
    O_FCVT_S_LU,
    O_FCVT_W_S,
    O_FCVT_WU_S,
    O_FCVT_L_S,
    O_FCVT_LU_S,
    O_FCVT_D_S,
    O_FCVT_S_D,
} riscv64_asm_opcode_t;


#define FLAG_MAX_COUNT 5

typedef struct {
    riscv64_asm_opcode_t op;
    int flags[FLAG_MAX_COUNT];
} riscv64_opr_flags; //riscv64_asm_opcode_flags;

typedef struct {
    int count;
    riscv64_opr_flags **list;
} riscv64_opr_flags_list; // arm64_asm_opcode_flags_list;

#define ZEROREG r_x0
#define RA r_x1
#define SP r_x2
#define GP r_x3
#define TP r_x4
#define T0 r_x5
#define T1 r_x6
#define T2 r_x7
#define FP r_x8
#define S1 r_x9
#define A0 r_x10
#define A1 r_x11
#define A2 r_x12
#define A3 r_x13
#define A4 r_x14
#define A5 r_x15
#define A6 r_x16
#define A7 r_x17
#define S2 r_x18
#define S3 r_x19
#define S4 r_x20
#define S5 r_x21
#define S6 r_x22
#define S7 r_x23
#define S8 r_x24
#define S9 r_x25
#define S10 r_x26
#define S11 r_x27
#define T3 r_x28
#define T4 r_x29
#define T5 r_x30
#define T6 r_x31

#define FT0 r_f0
#define FT1 r_f1
#define FT2 r_f2
#define FT3 r_f3
#define FT4 r_f4
#define FT5 r_f5
#define FT6 r_f6
#define FT7 r_f7
#define FS0 r_f8
#define FS1 r_f9
#define FA0 r_f10
#define FA1 r_f11
#define FA2 r_f12
#define FA3 r_f13
#define FA4 r_f14
#define FA5 r_f15
#define FA6 r_f16
#define FA7 r_f17
#define FS2 r_f18
#define FS3 r_f19
#define FS4 r_f20
#define FS5 r_f21
#define FS6 r_f22
#define FS7 r_f23
#define FS8 r_f24
#define FS9 r_f25
#define FS10 r_f26
#define FS11 r_f27
#define FT8 r_f28
#define FT9 r_f29
#define FT10 r_f30
#define FT11 r_f31

static const struct {
    const char *name;
    riscv64_round_mode_t mode;
} riscv64_round_modes[] = {
        {"rne", RISCV64_RNE},
        {"rtz", RISCV64_RTZ},
        {"rdn", RISCV64_RDN},
        {"rup", RISCV64_RUP},
        {"rmm", RISCV64_RMM},
};

typedef struct {
    char *name;
    bool is_local;
    int64_t offset; // 汇编器识别 offset
    //    asm_arm64_reloc_type reloc_type; // 重定位类型 todo
} riscv64_asm_operand_symbol_t;

typedef struct {
    riscv64_asm_operand_type_t type;
    uint8_t size;

    union {
        reg_t reg;
        int64_t immediate;
        struct {
            int64_t offset;
            reg_t *reg;
        } indirect;
        riscv64_round_mode_t round_mode;

        riscv64_asm_operand_symbol_t symbol;
    };
} riscv64_asm_operand_t;

// riscv64 在指令生成时长度不固定，从 16 ～ 64 不等, 所以在 inst 中冗余空间存储指令
typedef struct {
    uint64_t op_id;
    int line;
    int column;
    riscv64_asm_raw_opcode_t raw_opcode;
    riscv64_asm_opcode_t opcode;
    uint8_t count;
    riscv64_asm_operand_t *operands[4];

    uint8_t opcode_count;
    uint8_t opcode_data[26];
} riscv64_asm_inst_t;

void riscv64_match_opcode(riscv64_asm_inst_t *inst);

#endif //NATURE_ASM_H
