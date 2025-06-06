#include "debug.h"

#include <stdio.h>

#include "debug_asm.h"
#include "debug_lir.h"

int current_parser_line = 0;

string ast_type_to_str[] = {
        [AST_EXPR_LITERAL] = "AST_EXPR_TYPE_LITERAL",
        [AST_EXPR_BINARY] = "AST_EXPR_TYPE_BINARY",
        [AST_EXPR_UNARY] = "AST_EXPR_TYPE_UNARY",
        [AST_EXPR_IDENT] = "AST_EXPR_TYPE_IDENT",
        [AST_EXPR_STRUCT_SELECT] = "AST_EXPR_INSTANCE_ACCESS",
        [AST_EXPR_ENV_ACCESS] = "AST_EXPR_ENV_VALUE",
        [AST_EXPR_ACCESS] = "AST_EXPR_ACCESS",
        [AST_EXPR_SELECT] = "AST_EXPR_SELECT",
        [AST_EXPR_MAP_ACCESS] = "AST_EXPR_TYPE_ACCESS_MAP",
        [AST_EXPR_MAP_NEW] = "AST_EXPR_TYPE_NEW_MAP",
        [AST_EXPR_VEC_ACCESS] = "AST_EXPR_TYPE_ACCESS_LIST",
        [AST_EXPR_VEC_NEW] = "AST_EXPR_TYPE_NEW_LIST",
        [AST_VAR_DECL] = "AST_VAR_DECL",
        [AST_STMT_VARDEF] = "AST_STMT_VAR_DECL_ASSIGN",
        [AST_STMT_ASSIGN] = "AST_STMT_ASSIGN",
        [AST_STMT_GLOBAL_ASSIGN] = "AST_STMT_GLOBAL_ASSIGN",
        [AST_STMT_RETURN] = "AST_STMT_RETURN",
        [AST_STMT_IF] = "AST_STMT_IF",
        [AST_STMT_FOR_ITERATOR] = "AST_STMT_FOR_ITERATOR",
        [AST_FNDEF] = "AST_FUNCTION_DECL",
        [AST_CALL] = "AST_CALL",
        [AST_STMT_TYPEDEF] = "AST_STMT_TYPE_DECL",
        [AST_STMT_ENV_CLOSURE] = "AST_ENV_CLOSURE",
};

string token_type_to_str[] = {
        [TOKEN_LEFT_PAREN] = "TOKEN_LEFT_PAREN",
        [TOKEN_RIGHT_PAREN] = "TOKEN_RIGHT_PAREN", // ()
        [TOKEN_LEFT_SQUARE] = "TOKEN_LEFT_SQUARE",
        [TOKEN_RIGHT_SQUARE] = "TOKEN_RIGHT_SQUARE", // []
        [TOKEN_LEFT_CURLY] = "TOKEN_LEFT_CURLY",
        [TOKEN_RIGHT_CURLY] = "TOKEN_RIGHT_CURLY", // {}
        [TOKEN_LEFT_ANGLE] = "TOKEN_LEFT_ANGLE",
        [TOKEN_RIGHT_ANGLE] = "TOKEN_RIGHT_ANGLE", // <>

        [TOKEN_COMMA] = "TOKEN_COMMA",
        [TOKEN_DOT] = "TOKEN_DOT",
        [TOKEN_MINUS] = "TOKEN_MINUS",
        [TOKEN_PLUS] = "TOKEN_PLUS",
        [TOKEN_COLON] = "TOKEN_COLON",
        [TOKEN_SEMICOLON] = "TOKEN_SEMICOLON",
        [TOKEN_SLASH] = "TOKEN_SLASH",
        [TOKEN_STAR] = "TOKEN_STAR", // * STAR
        [TOKEN_IMPORT_STAR] = "TOKEN_IMPORT_STAR", // * STAR
        [TOKEN_QUESTION] = "TOKEN_QUESTION", // ?
        [TOKEN_EOF] = "TOKEN_EOF",
        [TOKEN_STMT_EOF] = "TOKEN_STMT_EOF",
        [TOKEN_RIGHT_ARROW] = "TOKEN_RIGHT_ARROW",

        // ONE OR TWO CHARACTER TOKENS.
        [TOKEN_NOT] = "TOKEN_NOT",
        [TOKEN_NOT_EQUAL] = "TOKEN_NOT_EQUAL",
        [TOKEN_EQUAL] = "TOKEN_EQUAL",
        [TOKEN_EQUAL_EQUAL] = "TOKEN_EQUAL_EQUAL",
        [TOKEN_GREATER_EQUAL] = "TOKEN_GREATER_EQUAL",
        [TOKEN_LESS_EQUAL] = "TOKEN_LESS_EQUAL",
        [TOKEN_AND_AND] = "TOKEN_AND_AND",
        [TOKEN_OR_OR] = "TOKEN_OR_OR",

        [TOKEN_PLUS_EQUAL] = "TOKEN_PLUS_EQUAL", // +=
        [TOKEN_MINUS_EQUAL] = "TOKEN_MINUS_EQUAL", // -=
        [TOKEN_STAR_EQUAL] = "TOKEN_STAR_EQUAL", // *=
        [TOKEN_SLASH_EQUAL] = "TOKEN_SLASH_EQUAL", // /=
        [TOKEN_PERSON_EQUAL] = "TOKEN_PERSON_EQUAL", // %=
        [TOKEN_AND_EQUAL] = "TOKEN_AND_EQUAL", // &=
        [TOKEN_OR_EQUAL] = "TOKEN_OR_EQUAL", // |=
        [TOKEN_XOR_EQUAL] = "TOKEN_XOR_EQUAL", // ^=
        [TOKEN_LEFT_SHIFT_EQUAL] = "TOKEN_LEFT_SHIFT_EQUAL", // >>=
        [TOKEN_RIGHT_SHIFT_EQUAL] = "TOKEN_RIGHT_SHIFT_EQUAL", // <<=

        // 位运算
        [TOKEN_TILDE] = "TOKEN_TILDE", // ~
        [TOKEN_AND] = "TOKEN_AND", // &
        [TOKEN_OR] = "TOKEN_OR", // |
        [TOKEN_XOR] = "TOKEN_XOR", // ^
        [TOKEN_LEFT_SHIFT] = "TOKEN_LEFT_SHIFT", // <<
        [TOKEN_RIGHT_SHIFT] = "TOKEN_RIGHT_SHIFT", // >>

        // LITERALS.
        [TOKEN_IDENT] = "TOKEN_IDENT",
        [TOKEN_LITERAL_STRING] = "TOKEN_LITERAL_STRING",
        [TOKEN_LITERAL_FLOAT] = "TOKEN_LITERAL_FLOAT",
        [TOKEN_LITERAL_INT] = "TOKEN_LITERAL_INT",

        // KEYWORDS.
        [TOKEN_TRUE] = "TOKEN_TRUE",
        [TOKEN_FALSE] = "TOKEN_FALSE",
        [TOKEN_TYPE] = "TOKEN_TYPE",
        [TOKEN_NULL] = "TOKEN_NULL",
        [TOKEN_VOID] = "TOKEN_VOID",
        [TOKEN_ANY] = "TOKEN_ANY",
        [TOKEN_STRUCT] = "TOKEN_STRUCT",
        [TOKEN_INTERFACE] = "TOKEN_INTERFACE",
        [TOKEN_FOR] = "TOKEN_FOR",
        [TOKEN_IN] = "TOKEN_IN",
        // [TOKEN_WHILE]="TOKEN_WHILE",
        [TOKEN_IF] = "TOKEN_IF",
        [TOKEN_ELSE] = "TOKEN_ELSE",
        [TOKEN_ELSE_IF] = "TOKEN_ELSE_IF",
        [TOKEN_VAR] = "TOKEN_VAR",
        [TOKEN_CONST] = "TOKEN_CONST",
        [TOKEN_STRING] = "TOKEN_STRING",
        [TOKEN_BOOL] = "TOKEN_BOOL",
        [TOKEN_FLOAT] = "TOKEN_FLOAT",
        [TOKEN_INT] = "TOKEN_INT",
        // [TOKEN_ARRAY]="TOKEN_ARRAY",
        // [TOKEN_MAP]="TOKEN_MAP",
        [TOKEN_FN] = "TOKEN_FN",
        [TOKEN_IMPORT] = "TOKEN_IMPORT",
        [TOKEN_AS] = "TOKEN_AS",
        [TOKEN_RETURN] = "TOKEN_RETURN"};

string lir_opcode_to_string[] = {
        [LIR_OPCODE_ADD] = "ADD  ",
        [LIR_OPCODE_SUB] = "SUB  ",
        [LIR_OPCODE_MUL] = "MUL  ",
        [LIR_OPCODE_UDIV] = "UDIV  ",
        [LIR_OPCODE_UREM] = "UREM  ",
        [LIR_OPCODE_SDIV] = "SDIV  ",
        [LIR_OPCODE_SREM] = "SREM  ",
        [LIR_OPCODE_SLT] = "SLT   ",
        [LIR_OPCODE_USLT] = "USLT   ",
        [LIR_OPCODE_SLE] = "SLE  ",
        [LIR_OPCODE_SGT] = "SGT   ",
        [LIR_OPCODE_SGE] = "SGE  ",
        [LIR_OPCODE_SEE] = "SEE ",
        [LIR_OPCODE_SNE] = "SNE ",
        [LIR_OPCODE_NEG] = "NEG   ",
        [LIR_OPCODE_LEA] = "LEA   ",
        // 位运算
        [LIR_OPCODE_NOT] = "NOT  ",
        [LIR_OPCODE_XOR] = "XOR  ",
        [LIR_OPCODE_OR] = "OR  ",
        [LIR_OPCODE_AND] = "AND  ",
        [LIR_OPCODE_USHR] = "USHR  ",
        [LIR_OPCODE_SSHR] = "SSHR  ",
        [LIR_OPCODE_USHL] = "USHL  ",

        [LIR_OPCODE_PHI] = "PHI  ",
        [LIR_OPCODE_MOVE] = "MOVE ",
        [LIR_OPCODE_UEXT] = "UEXT ",
        [LIR_OPCODE_SEXT] = "SEXT ",
        [LIR_OPCODE_TRUNC] = "TRUNC",
        [LIR_OPCODE_BEQ] = "BEQ",
        [LIR_OPCODE_BAL] = "BAL ",
        [LIR_OPCODE_PUSH] = "PUSH  ",
        [LIR_OPCODE_POP] = "POP   ",
        [LIR_OPCODE_CALL] = "CALL  ",
        [LIR_OPCODE_RT_CALL] = "R_CALL",
        [LIR_OPCODE_CLR] = "CLR    ",
        [LIR_OPCODE_CLV] = "CLV    ",
        [LIR_OPCODE_NOP] = "NOP    ",
        [LIR_OPCODE_RETURN] = "RETURN ",
        [LIR_OPCODE_BREAK] = "BREAK ",
        [LIR_OPCODE_LABEL] = "LABEL ",
        [LIR_OPCODE_FN_BEGIN] = "FN_BEGIN",
        [LIR_OPCODE_FN_END] = "FN_END",
        [LIR_OPCODE_SAFEPOINT] = "SAFEPOINT",
};

void debug_parser(int line, char *token) {
    if (current_parser_line != line) {
        current_parser_line = line;
        printf("\n");
        printf("%d:", line);
    }
    printf("%s ", token);
    fflush(stdout);
}

void debug_parser_stmt(ast_type_t t) {
    printf("\n[DEBUG] PARSER stmt: %s\n", ast_type_to_str[t]);
}

void debug_scanner(token_t *t) {
    printf("[DEBUG] SCANNER line:%d, %s: %s \n", t->line, token_type_to_str[t->type], t->literal);
}

void debug_stmt(string type, ast_stmt_t stmt) {
    printf("[DEBUG] %s line: %d, stmt: %s\n", type, stmt.line, ast_type_to_str[stmt.assert_type]);
}

/**
 * c->operations
 * @param c
 */
void debug_lir(closure_t *c, char *key) {
#ifdef DEBUG_LIR
    // 跳过各种全局的 init 方法
    if (!starts_with(c->fndef->symbol_name, DEBUG_LIR)) {
        return;
    }

    printf("%s lir: %s ---------------------------------------------------------------------\n",
           key,
           c->fndef->symbol_name);
    linked_node *current = c->operations->front;
    while (current->value != NULL) {
        lir_op_t *op = current->value;
        //        printf("%d", op->id);
        if (op->code == LIR_OPCODE_LABEL) {
            printf("%s\t", lir_opcode_to_string[op->code]);
        } else {
            printf("\t\t%s\t", lir_opcode_to_string[op->code]);
        }

        // first
        if (op->first) {
            printf("%s", lir_operand_to_string(op->first));
        }
        if (op->second) {
            if (op->first) {
                printf(", ");
            }
            printf("%s", lir_operand_to_string(op->second));
        }
        if (op->output) {
            if (op->first) {
                printf(" -> ");
            }
            printf("%s", lir_operand_to_string(op->output));
        }
        printf("\n");

        current = current->succ;
    }
    fflush(stdout);
    printf("\n\n");
#endif
}

/**
 * 遍历展示 blocks
 * @param c
 */
void debug_block_lir(closure_t *c, char *stage_after) {
#ifdef DEBUG_LIR
    if (!starts_with(c->linkident, DEBUG_LIR)) {
        return;
    }

    printf("%s after block_lir: %s------------------------------------------------------------------------\n",
           stage_after, c->fndef->symbol_name);
    for (int i = 0; i < c->blocks->count; ++i) {
        basic_block_t *basic_block = c->blocks->take[i];
        debug_basic_block(basic_block);
    }
    printf("\n\n");

    fflush(stdout);
#endif
}

void debug_interval_var(interval_t *interval, char *stage) {
    assert(interval);
    int parent_index = 0;
    char *parent_ident = "";
    int64_t stack_slot = 0;
    char *ranges = "";
    char *use_pos = "";
    char *type_str = "int";
    if (interval->alloc_type == LIR_FLAG_ALLOC_FLOAT) {
        type_str = "float";
    }

    if (interval->parent) {
        parent_index = interval->parent->index;
        parent_ident = interval->parent->var->ident;
    }

    if (interval->stack_slot) {
        stack_slot = *interval->stack_slot;
    }

    LINKED_FOR(interval->ranges) {
        interval_range_t *r = LINKED_VALUE();
        char *temp_range = dsprintf("[%d,%d)\t", r->from, r->to);
        ranges = str_connect(ranges, temp_range);
    }
    LINKED_FOR(interval->use_pos_list) {
        use_pos_t *u = LINKED_VALUE();
        char *temp_use = dsprintf("%d-%d\t", u->value, u->kind);
        use_pos = str_connect(use_pos, temp_use);
    }

    log_debug("%s var: index(%d-%s), parent(%d-%s), assigned=(%d(%s)-%s), stack_slot=%ld, ranges=%s, use_pos=%s",
              stage,
              interval->index,
              interval->var ? interval->var->ident : "-", parent_index, parent_ident,
              interval->assigned,
              alloc_regs[interval->assigned] ? alloc_regs[interval->assigned]->name : "_",
              type_str,
              stack_slot, ranges,
              use_pos);
}

void debug_closure_interval(closure_t *c, char *stage) {
#ifdef DEBUG_INTERVAL
    if (!strstr(c->linkident, DEBUG_INTERVAL)) {
        return;
    }

    log_debug("stage=%s closure=%s interval ------------------------------------------------------------------------",
              stage,
              c->linkident);
    for (int reg_id = 1; reg_id < alloc_reg_count(); ++reg_id) {
        reg_t *reg = alloc_regs[reg_id];
        interval_t *interval = table_get(c->interval_table, reg->name);
        assert(interval);
        int parent_index = 0;
        char *parent_ident = "";
        int64_t stack_slot = 0;
        char *ranges = "";
        char *use_pos = "";
        char *type_str = "int";
        if (interval->alloc_type == LIR_FLAG_ALLOC_FLOAT) {
            type_str = "float";
        }

        if (interval->parent) {
            parent_index = interval->parent->index;
            parent_ident = interval->parent->var->ident;
        }

        if (interval->stack_slot) {
            stack_slot = *interval->stack_slot;
        }

        LINKED_FOR(interval->ranges) {
            interval_range_t *r = LINKED_VALUE();
            char *temp_range = dsprintf("[%d,%d)\t", r->from, r->to);
            ranges = str_connect(ranges, temp_range);
        }
        LINKED_FOR(interval->use_pos_list) {
            use_pos_t *u = LINKED_VALUE();
            char *temp_use = dsprintf("%d-%d\t", u->value, u->kind);
            use_pos = str_connect(use_pos, temp_use);
        }

        log_debug("reg: index(%d-%s), parent(%d-%s), assigned=(%d-%s), stack_slot=%ld, ranges=%s, use_pos=%s",
                  interval->index, reg->name,
                  parent_index, parent_ident, !interval->spilled ? interval->assigned : -1, type_str, stack_slot,
                  ranges,
                  use_pos);
    }

    for (int i = 0; i < c->var_defs->count; ++i) {
        lir_var_t *var = c->var_defs->take[i];
        interval_t *interval = table_get(c->interval_table, var->ident);
        debug_interval_var(interval, stage);
    }

    printf("\n\n");
    fflush(stdout);
#endif
}

/**
 * block: as
 *     move....
 *     xxx...
 *     xxx..
 * succ: name1, name2
 * pred: name1, name2
 *
 *
 * @param block
 */
void debug_basic_block(basic_block_t *block) {
    // block as, ops, succ, pred
    printf("block: %s\t\t", block->name);
    if (block->loop.index != -1) {
        printf("%s", block->loop.header ? "loop_header|" : "");
        printf("%s", block->loop.end ? "loop_end|" : "");
        printf("loop:i-%d/d-%d", block->loop.index, block->loop.depth);
    }
    printf("\n");

    linked_node *current = block->operations->front;
    while (current->value != NULL) {
        lir_op_t *op = current->value;
        printf("%d", op->id);
        if (op->is_resolve) {
            printf("\t\t%s!\t", lir_opcode_to_string[op->code]);
        } else {
            printf("\t\t%s\t", lir_opcode_to_string[op->code]);
        }

        // first
        if (op->first) {
            printf("%s", lir_operand_to_string(op->first));
        }
        if (op->second) {
            if (op->first) {
                printf(", ");
            }
            printf("%s", lir_operand_to_string(op->second));
        }
        if (op->output) {
            if (op->first) {
                printf(" -> ");
            }
            printf("%s", lir_operand_to_string(op->output));
        }
        printf("\n");

        current = current->succ;
    }

    printf("\n\t\tpred:");
    for (int i = 0; i < block->preds->count; ++i) {
        printf("%s\t", ((basic_block_t *) block->preds->take[i])->name);
    }
    printf("\n\t\tsucc:");
    for (int i = 0; i < block->succs->count; ++i) {
        printf("%s\t", ((basic_block_t *) block->succs->take[i])->name);
    }
    printf("\n\t\tlive:");
    for (int i = 0; i < block->temp_live_in->count; ++i) {
        lir_var_t *var = block->temp_live_in->take[i];
        printf("%s\t", var->ident);
    }
    printf("\n\t\tlive_in:");
    for (int i = 0; i < block->live_in->count; ++i) {
        lir_var_t *var = block->live_in->take[i];
        printf("%s\t", var->ident);
    }
    printf("\n\n\n");
}

void debug_asm(closure_t *c) {
#ifdef DEBUG_ASM
    if (!starts_with(c->linkident, DEBUG_ASM)) {
        return;
    }

    printf("asm: %s------------------------------------------------------------------------\n", c->fndef->symbol_name);
    for (int i = 0; i < c->asm_operations->count; ++i) {
        asm_op_to_string(i, c->asm_operations->take[i]);
    }
    fflush(stdout);
#endif
}
