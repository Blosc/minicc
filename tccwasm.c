/*
 *  wasm module output for TCC (restricted backend)
 */

#define USING_GLOBALS
#include "tcc.h"
#include "wasm-backend.h"

typedef struct WasmBuf {
    unsigned char *data;
    int len;
    int cap;
} WasmBuf;

typedef struct WasmMemLayout {
    int rodata_base;
    int data_base;
    int bss_base;
    int stack_top;
} WasmMemLayout;

static WasmMemLayout wasm_layout;
static Section *wasm_sec_text;
static Section *wasm_sec_data;
static Section *wasm_sec_rodata;
static Section *wasm_sec_bss;

typedef struct WasmSig {
    unsigned char ret_type;
    unsigned char nb_params;
    unsigned char param_types[WASM_MAX_CALL_ARGS];
} WasmSig;

static int wasm_align_up(int v, int a)
{
    return (v + a - 1) & -a;
}

static void wb_reserve(WasmBuf *b, int add)
{
    int need = b->len + add;
    int n;
    if (need <= b->cap)
        return;
    n = b->cap ? b->cap : 256;
    while (n < need)
        n = n * 2;
    b->data = tcc_realloc(b->data, n);
    b->cap = n;
}

static void wb_u8(WasmBuf *b, int v)
{
    wb_reserve(b, 1);
    b->data[b->len++] = (unsigned char)v;
}

static void wb_mem(WasmBuf *b, const void *p, int n)
{
    wb_reserve(b, n);
    memcpy(b->data + b->len, p, n);
    b->len += n;
}

static void wb_uleb(WasmBuf *b, unsigned v)
{
    do {
        unsigned c = v & 0x7f;
        v >>= 7;
        if (v)
            c |= 0x80;
        wb_u8(b, c);
    } while (v);
}

static void wb_sleb(WasmBuf *b, int v)
{
    int more = 1;
    while (more) {
        int c = v & 0x7f;
        int sign = c & 0x40;
        v >>= 7;
        more = !((v == 0 && !sign) || (v == -1 && sign));
        if (more)
            c |= 0x80;
        wb_u8(b, c);
    }
}

static void wb_sleb64(WasmBuf *b, int64_t v)
{
    int more = 1;
    while (more) {
        int c = (int)(v & 0x7f);
        int sign = c & 0x40;
        v >>= 7;
        more = !((v == 0 && !sign) || (v == -1 && sign));
        if (more)
            c |= 0x80;
        wb_u8(b, c);
    }
}

static void wb_f64(WasmBuf *b, double x)
{
    union {
        double d;
        unsigned char c[8];
    } u;
    u.d = x;
    wb_mem(b, u.c, 8);
}

static int wasm_valtype_byte(int t)
{
    switch (t) {
    case WASM_VAL_I32: return 0x7f;
    case WASM_VAL_I64: return 0x7e;
    case WASM_VAL_F32: return 0x7d;
    case WASM_VAL_F64: return 0x7c;
    default:
        tcc_error("wasm32 backend: invalid wasm value type %d", t);
        return 0x40;
    }
}

static void wb_local_get(WasmBuf *b, int idx) { wb_u8(b, 0x20), wb_uleb(b, idx); }
static void wb_local_set(WasmBuf *b, int idx) { wb_u8(b, 0x21), wb_uleb(b, idx); }
static void wb_local_tee(WasmBuf *b, int idx) { wb_u8(b, 0x22), wb_uleb(b, idx); }
static void wb_global_get(WasmBuf *b, int idx) { wb_u8(b, 0x23), wb_uleb(b, idx); }
static void wb_global_set(WasmBuf *b, int idx) { wb_u8(b, 0x24), wb_uleb(b, idx); }
static void wb_i32_const(WasmBuf *b, int v) { wb_u8(b, 0x41), wb_sleb(b, v); }
static void wb_i64_const(WasmBuf *b, int64_t v) { wb_u8(b, 0x42), wb_sleb64(b, v); }
static void wb_f64_const(WasmBuf *b, double v) { wb_u8(b, 0x44), wb_f64(b, v); }

static void wb_memarg(WasmBuf *b, int align_log2)
{
    wb_uleb(b, align_log2);
    wb_uleb(b, 0);
}

static int wasm_i32_reg_local(int reg, int local_i0)
{
    if (reg < 0 || reg > 3)
        tcc_error("wasm32 backend: invalid integer register %d", reg);
    return local_i0 + reg;
}

static int wasm_f64_reg_local(int reg, int local_f0)
{
    if (reg < 4 || reg > 7)
        tcc_error("wasm32 backend: invalid floating register %d", reg);
    return local_f0 + (reg - 4);
}

static int wasm_i32_bin_opcode(int op)
{
    switch (op) {
    case '+': return 0x6a;
    case '-': return 0x6b;
    case '*': return 0x6c;
    case '/': return 0x6d;
    case TOK_UDIV:
    case TOK_PDIV: return 0x6e;
    case '%': return 0x6f;
    case TOK_UMOD: return 0x70;
    case '&': return 0x71;
    case '|': return 0x72;
    case '^': return 0x73;
    case TOK_SHL: return 0x74;
    case TOK_SAR: return 0x75;
    case TOK_SHR: return 0x76;
    }
    tcc_error("wasm32 backend: unsupported i32 binop token %d", op);
    return 0x6a;
}

static int wasm_i32_cmp_opcode(int op)
{
    switch (op) {
    case TOK_EQ: return 0x46;
    case TOK_NE: return 0x47;
    case TOK_LT: return 0x48;
    case TOK_ULT: return 0x49;
    case TOK_GT: return 0x4a;
    case TOK_UGT: return 0x4b;
    case TOK_LE: return 0x4c;
    case TOK_ULE: return 0x4d;
    case TOK_GE: return 0x4e;
    case TOK_UGE: return 0x4f;
    }
    tcc_error("wasm32 backend: unsupported i32 cmp token %d", op);
    return 0x46;
}

static int wasm_f32_cmp_opcode(int op)
{
    switch (op) {
    case TOK_EQ: return 0x5b;
    case TOK_NE: return 0x5c;
    case TOK_LT:
    case TOK_ULT: return 0x5d;
    case TOK_GT:
    case TOK_UGT: return 0x5e;
    case TOK_LE:
    case TOK_ULE: return 0x5f;
    case TOK_GE:
    case TOK_UGE: return 0x60;
    }
    tcc_error("wasm32 backend: unsupported f32 cmp token %d", op);
    return 0x5b;
}

static int wasm_f64_cmp_opcode(int op)
{
    switch (op) {
    case TOK_EQ: return 0x61;
    case TOK_NE: return 0x62;
    case TOK_LT:
    case TOK_ULT: return 0x63;
    case TOK_GT:
    case TOK_UGT: return 0x64;
    case TOK_LE:
    case TOK_ULE: return 0x65;
    case TOK_GE:
    case TOK_UGE: return 0x66;
    }
    tcc_error("wasm32 backend: unsupported f64 cmp token %d", op);
    return 0x61;
}

static int wasm_f_bin_opcode(int op, int is_f32)
{
    switch (op) {
    case '+': return is_f32 ? 0x92 : 0xa0;
    case '-': return is_f32 ? 0x93 : 0xa1;
    case '*': return is_f32 ? 0x94 : 0xa2;
    case '/': return is_f32 ? 0x95 : 0xa3;
    }
    tcc_error("wasm32 backend: unsupported floating binop token %d", op);
    return is_f32 ? 0x92 : 0xa0;
}

static int wasm_find_func_index_by_tok(int tok)
{
    int i;
    for (i = 0; i < tcc_wasm_nb_funcs; ++i) {
        if (tcc_wasm_funcs[i].sym && tcc_wasm_funcs[i].sym->v == tok)
            return i;
    }
    return -1;
}

static int wasm_find_func_index_by_sym_index(int sym_index)
{
    int i;
    for (i = 0; i < tcc_wasm_nb_funcs; ++i) {
        Sym *fsym = tcc_wasm_funcs[i].sym;
        if (fsym && fsym->c == sym_index)
            return i;
    }
    return -1;
}

static int wasm_find_func_index_by_name(const char *name)
{
    int i;
    if (!name || !*name)
        return -1;
    for (i = 0; i < tcc_wasm_nb_funcs; ++i) {
        const char *fname = tcc_wasm_funcs[i].name;
        if (fname && !strcmp(fname, name))
            return i;
    }
    return -1;
}

static int wasm_find_defined_sym_index_by_name(const char *name)
{
    ElfSym *symtab, *es;
    Section *strsec;
    int i, n, best = -1;

    if (!name || !*name || !symtab_section || !symtab_section->data)
        return -1;

    strsec = symtab_section->link;
    if (!strsec || !strsec->data)
        return -1;

    symtab = (ElfSym *)symtab_section->data;
    n = symtab_section->data_offset / sizeof(ElfSym);
    for (i = 1; i < n; ++i) {
        const char *sname;
        es = &symtab[i];
        if (es->st_shndx == SHN_UNDEF || es->st_name >= strsec->data_offset)
            continue;
        sname = (const char *)strsec->data + es->st_name;
        if (strcmp(sname, name))
            continue;
        if (ELFW(ST_TYPE)(es->st_info) != STT_SECTION)
            return i;
        if (best < 0)
            best = i;
    }
    return best;
}

static int wasm_func_ptr_value_from_sym_index(int sym_index, const char *name, int addend)
{
    int fi;
    if (addend)
        tcc_error("wasm32 backend: function pointer arithmetic is not supported");
    fi = wasm_find_func_index_by_sym_index(sym_index);
    if (fi < 0)
        fi = wasm_find_func_index_by_name(name);
    if (fi < 0)
        tcc_error("wasm32 backend: unresolved function symbol '%s'",
                  name ? name : "?");
    return fi + 1; /* keep table index 0 as null */
}

static void wasm_push_i64_from_i32_pair(WasmBuf *b, int lo_local, int hi_local)
{
    wb_local_get(b, lo_local);
    wb_u8(b, 0xad); /* i64.extend_i32_u */
    wb_local_get(b, hi_local);
    wb_u8(b, 0xad); /* i64.extend_i32_u */
    wb_i64_const(b, 32);
    wb_u8(b, 0x86); /* i64.shl */
    wb_u8(b, 0x84); /* i64.or */
}

static void wasm_store_i64_to_i32_pair(WasmBuf *b, int local_tmp64, int lo_local, int hi_local)
{
    wb_local_tee(b, local_tmp64);
    wb_u8(b, 0xa7); /* i32.wrap_i64 */
    wb_local_set(b, lo_local);
    wb_local_get(b, local_tmp64);
    wb_i64_const(b, 32);
    wb_u8(b, 0x88); /* i64.shr_u */
    wb_u8(b, 0xa7); /* i32.wrap_i64 */
    wb_local_set(b, hi_local);
}

static int wasm_sym_addr(Sym *sym, int addend)
{
    ElfSym *es;
    Sym *ps;
    const char *name = NULL;
    int sym_index;
    int sh;
    int base = 0;

    if (!sym)
        return addend;
    sym_index = sym->c;
    es = elfsym(sym);
    sh = es->st_shndx;

    if (sh == SHN_UNDEF) {
        int si;
        for (ps = sym->prev_tok; ps; ps = ps->prev_tok) {
            if (!ps->c)
                continue;
            es = elfsym(ps);
            if (es->st_shndx != SHN_UNDEF) {
                sym = ps;
                sh = es->st_shndx;
                break;
            }
        }
        if (sh == SHN_UNDEF && sym->v >= TOK_IDENT) {
            for (ps = sym_find(sym->v); ps; ps = ps->prev_tok) {
                if (!ps->c)
                    continue;
                es = elfsym(ps);
                if (es->st_shndx != SHN_UNDEF) {
                    sym = ps;
                    sh = es->st_shndx;
                    break;
                }
            }
        }
        if (sh == SHN_UNDEF && sym->v >= TOK_IDENT) {
            name = get_tok_str(sym->v, NULL);
            si = wasm_find_defined_sym_index_by_name(name);
            if (si > 0) {
                sym_index = si;
                es = &((ElfSym *)symtab_section->data)[si];
                sh = es->st_shndx;
            }
        }
        if (sh == SHN_UNDEF) {
            int bt = sym->type.t & VT_BTYPE;
            if (bt == VT_FUNC
                || (bt == VT_PTR && sym->type.ref
                    && (sym->type.ref->type.t & VT_BTYPE) == VT_FUNC)) {
                return wasm_func_ptr_value_from_sym_index(sym_index, name, addend);
            }
        }
        if (sh == SHN_UNDEF && (!name || !*name)) {
            int t = sym->type.t;
            int ro_size = wasm_sec_rodata ? (int)wasm_sec_rodata->data_offset : 0;
            int data_size = wasm_sec_data ? (int)wasm_sec_data->data_offset : 0;
            int bss_size = wasm_sec_bss ? (int)wasm_sec_bss->data_offset : 0;
            if ((t & VT_STATIC) && (((t & VT_BTYPE) == VT_PTR) || (t & VT_ARRAY))) {
                if (addend >= 0 && addend < ro_size)
                    return wasm_layout.rodata_base + addend;
                if (addend >= 0 && addend < data_size)
                    return wasm_layout.data_base + addend;
                if (addend >= 0 && addend < bss_size)
                    return wasm_layout.bss_base + addend;
            }
        }
    }
    if (sh == SHN_UNDEF)
        tcc_error("wasm32 backend: unresolved symbol (tok=%d)", sym->v);

    if (wasm_sec_rodata && sh == wasm_sec_rodata->sh_num)
        base = wasm_layout.rodata_base;
    else if (wasm_sec_data && sh == wasm_sec_data->sh_num)
        base = wasm_layout.data_base;
    else if (wasm_sec_bss && sh == wasm_sec_bss->sh_num)
        base = wasm_layout.bss_base;
    else if (wasm_sec_text && sh == wasm_sec_text->sh_num)
        return wasm_func_ptr_value_from_sym_index(sym_index, name, addend);
    else
        tcc_error("wasm32 backend: unsupported symbol section for '%s'", get_tok_str(sym->v, NULL));

    return base + (int)es->st_value + addend;
}

static int wasm_sym_addr_from_elfsym(int sym_index, int addend)
{
    ElfSym *es;
    ElfSym *symtab;
    Section *strsec;
    const char *name = NULL;
    int sh;
    int base = 0;

    if (sym_index <= 0)
        return addend;
    if (!symtab_section || !symtab_section->data)
        tcc_error("wasm32 backend: missing symbol table for relocation");
    if ((unsigned)sym_index >= (unsigned)(symtab_section->data_offset / sizeof(ElfSym)))
        tcc_error("wasm32 backend: bad relocation symbol index %d", sym_index);

    symtab = (ElfSym *)symtab_section->data;
    es = &symtab[sym_index];
    sh = es->st_shndx;

    strsec = symtab_section->link;
    if (strsec && strsec->data && es->st_name < strsec->data_offset)
        name = (const char *)strsec->data + es->st_name;

    if (sh == SHN_UNDEF || (wasm_sec_text && sh == wasm_sec_text->sh_num))
        return wasm_func_ptr_value_from_sym_index(sym_index, name, addend);

    if (wasm_sec_rodata && sh == wasm_sec_rodata->sh_num)
        base = wasm_layout.rodata_base;
    else if (wasm_sec_data && sh == wasm_sec_data->sh_num)
        base = wasm_layout.data_base;
    else if (wasm_sec_bss && sh == wasm_sec_bss->sh_num)
        base = wasm_layout.bss_base;
    else if (sh == SHN_ABS)
        return (int)es->st_value + addend;
    else
        tcc_error("wasm32 backend: unsupported relocation section for symbol '%s'",
                  name ? name : "?");

    return base + (int)es->st_value + addend;
}

static int wasm_sym_addr_from_tok(int tok, int addend)
{
    const char *name;
    int si;

    if (tok < TOK_IDENT)
        return addend;
    name = get_tok_str(tok, NULL);
    si = wasm_find_defined_sym_index_by_name(name);
    if (si > 0)
        return wasm_sym_addr_from_elfsym(si, addend);
    return wasm_func_ptr_value_from_sym_index(0, name, addend);
}

static int wasm_sym_addr_from_op(WasmOp *op)
{
    if (op->sym_index > 0)
        return wasm_sym_addr_from_elfsym(op->sym_index, op->imm);
    if (op->sym_tok >= TOK_IDENT)
        return wasm_sym_addr_from_tok(op->sym_tok, op->imm);
    return wasm_sym_addr(op->sym, op->imm);
}

static void wasm_apply_data_relocs(Section *s)
{
    Section *sr;
    ElfW_Rel *rel;
    int i, n;

    if (!s || !s->data)
        return;
    sr = s->reloc;
    if (!sr || !sr->data)
        return;

    n = sr->data_offset / sizeof(ElfW_Rel);
    for (i = 0; i < n; ++i) {
        unsigned long off;
        unsigned char *ptr;
        int sym_index;
        int addend;
        int value;

        rel = (ElfW_Rel *)sr->data + i;
        off = rel->r_offset;
        if (off + 4 > (unsigned long)s->data_offset)
            tcc_error("wasm32 backend: relocation offset out of range in section '%s'", s->name);

        ptr = s->data + off;
        sym_index = ELFW(R_SYM)(rel->r_info);
#if SHT_RELX == SHT_RELA
        addend = rel->r_addend;
#else
        addend = (int)(int32_t)read32le(ptr);
#endif
        value = wasm_sym_addr_from_elfsym(sym_index, addend);
        write32le(ptr, value);
    }
}

static int wasm_sig_matches_func(WasmSig *sig, WasmFuncIR *f)
{
    if (sig->ret_type != f->ret_type || sig->nb_params != f->nb_params)
        return 0;
    return !memcmp(sig->param_types, f->param_types, f->nb_params);
}

static int wasm_sig_matches_op(WasmSig *sig, WasmOp *op)
{
    if (sig->ret_type != op->type || sig->nb_params != op->call_nb_args)
        return 0;
    return !memcmp(sig->param_types, op->call_arg_type, op->call_nb_args);
}

static int wasm_pc_to_index(WasmFuncIR *f, int pc)
{
    int idx = pc - f->start_pc;
    if (pc == f->end_pc)
        return f->nb_ops;
    if (idx < 0 || idx >= f->nb_ops)
        tcc_error("wasm32 backend: bad jump target pc=%d (range %d..%d)",
                  pc, f->start_pc, f->end_pc);
    return idx;
}

static void wasm_emit_addr(WasmBuf *b, WasmOp *op, int local_fp, int local_i0)
{
    switch (op->flags & 0x00ff) {
    case WASM_ADDR_REG:
        wb_local_get(b, wasm_i32_reg_local(op->r1, local_i0));
        if (op->imm)
            wb_i32_const(b, op->imm), wb_u8(b, 0x6a);
        break;
    case WASM_ADDR_FP:
        wb_local_get(b, local_fp);
        if (op->imm)
            wb_i32_const(b, op->imm), wb_u8(b, 0x6a);
        break;
    case WASM_ADDR_ABS:
        wb_i32_const(b, op->imm);
        break;
    case WASM_ADDR_SYM:
        wb_i32_const(b, wasm_sym_addr_from_op(op));
        break;
    default:
        tcc_error("wasm32 backend: invalid address mode %u", op->flags & 0x00ff);
    }
}

static void wasm_emit_call_arg(WasmBuf *b, WasmOp *op, int i, int local_fp, int local_i0, int local_f0)
{
    int at = op->call_arg_type[i];
    int ar = op->call_arg_reg[i];

    if (ar == WASM_ARG_STACK) {
        wb_local_get(b, local_fp);
        if (op->call_arg_off[i])
            wb_i32_const(b, op->call_arg_off[i]), wb_u8(b, 0x6a);
        if (at == WASM_VAL_I32)
            wb_u8(b, 0x28), wb_memarg(b, 2);
        else if (at == WASM_VAL_I64)
            wb_u8(b, 0x29), wb_memarg(b, 3);
        else if (at == WASM_VAL_F32)
            wb_u8(b, 0x2a), wb_memarg(b, 2);
        else if (at == WASM_VAL_F64)
            wb_u8(b, 0x2b), wb_memarg(b, 3);
        else
            tcc_error("wasm32 backend: invalid call argument type %d", at);
        return;
    }

    if (at == WASM_VAL_I32)
        wb_local_get(b, wasm_i32_reg_local(ar, local_i0));
    else if (at == WASM_VAL_I64) {
        int ah = op->call_arg_hi[i];
        if (ah >= VT_CONST)
            tcc_error("wasm32 backend: malformed i64 call argument");
        wasm_push_i64_from_i32_pair(b,
            wasm_i32_reg_local(ar, local_i0),
            wasm_i32_reg_local(ah, local_i0));
    } else if (at == WASM_VAL_F32)
        wb_local_get(b, wasm_f64_reg_local(ar, local_f0)), wb_u8(b, 0xb6);
    else if (at == WASM_VAL_F64)
        wb_local_get(b, wasm_f64_reg_local(ar, local_f0));
    else
        tcc_error("wasm32 backend: invalid call argument type %d", at);
}

static int wasm_emit_libcall(WasmBuf *b, WasmOp *op,
                             int local_fp, int local_i0, int local_f0, int local_tmp64)
{
    int tok = op->call_tok;

    switch (tok) {
    case TOK___divdi3:
    case TOK___udivdi3:
    case TOK___moddi3:
    case TOK___umoddi3:
        if (op->call_nb_args != 2)
            tcc_error("wasm32 backend: invalid helper arity for %s", get_tok_str(tok, NULL));
        wasm_emit_call_arg(b, op, 0, local_fp, local_i0, local_f0);
        wasm_emit_call_arg(b, op, 1, local_fp, local_i0, local_f0);
        if (tok == TOK___divdi3) wb_u8(b, 0x7f);      /* i64.div_s */
        else if (tok == TOK___udivdi3) wb_u8(b, 0x80);/* i64.div_u */
        else if (tok == TOK___moddi3) wb_u8(b, 0x81); /* i64.rem_s */
        else wb_u8(b, 0x82);                          /* i64.rem_u */
        wasm_store_i64_to_i32_pair(b, local_tmp64,
            wasm_i32_reg_local(REG_IRET, local_i0),
            wasm_i32_reg_local(REG_IRE2, local_i0));
        return 1;

    case TOK___ashldi3:
    case TOK___lshrdi3:
    case TOK___ashrdi3:
        if (op->call_nb_args != 2)
            tcc_error("wasm32 backend: invalid helper arity for %s", get_tok_str(tok, NULL));
        wasm_emit_call_arg(b, op, 0, local_fp, local_i0, local_f0); /* i64 */
        wasm_emit_call_arg(b, op, 1, local_fp, local_i0, local_f0); /* i32 */
        if (op->call_arg_type[1] != WASM_VAL_I64)
            wb_u8(b, 0xad); /* i64.extend_i32_u */
        if (tok == TOK___ashldi3) wb_u8(b, 0x86);      /* i64.shl */
        else if (tok == TOK___lshrdi3) wb_u8(b, 0x88); /* i64.shr_u */
        else wb_u8(b, 0x87);                           /* i64.shr_s */
        wasm_store_i64_to_i32_pair(b, local_tmp64,
            wasm_i32_reg_local(REG_IRET, local_i0),
            wasm_i32_reg_local(REG_IRE2, local_i0));
        return 1;

    case TOK___floatundisf:
        if (op->call_nb_args != 1)
            tcc_error("wasm32 backend: invalid helper arity for %s", get_tok_str(tok, NULL));
        wasm_emit_call_arg(b, op, 0, local_fp, local_i0, local_f0);
        wb_u8(b, 0xb5); /* f32.convert_i64_u */
        wb_u8(b, 0xbb); /* f64.promote_f32 */
        wb_local_set(b, wasm_f64_reg_local(REG_FRET, local_f0));
        return 1;

    case TOK___floatundidf:
    case TOK___floatundixf:
        if (op->call_nb_args != 1)
            tcc_error("wasm32 backend: invalid helper arity for %s", get_tok_str(tok, NULL));
        wasm_emit_call_arg(b, op, 0, local_fp, local_i0, local_f0);
        wb_u8(b, 0xba); /* f64.convert_i64_u */
        wb_local_set(b, wasm_f64_reg_local(REG_FRET, local_f0));
        return 1;

    case TOK___fixunssfdi:
    case TOK___fixunsdfdi:
    case TOK___fixunsxfdi:
        if (op->call_nb_args != 1)
            tcc_error("wasm32 backend: invalid helper arity for %s", get_tok_str(tok, NULL));
        if (tok == TOK___fixunssfdi && op->call_arg_type[0] == WASM_VAL_F32) {
            wasm_emit_call_arg(b, op, 0, local_fp, local_i0, local_f0);
            wb_u8(b, 0xaf); /* i64.trunc_f32_u */
        } else {
            wasm_emit_call_arg(b, op, 0, local_fp, local_i0, local_f0);
            wb_u8(b, 0xb1); /* i64.trunc_f64_u */
        }
        wasm_store_i64_to_i32_pair(b, local_tmp64,
            wasm_i32_reg_local(REG_IRET, local_i0),
            wasm_i32_reg_local(REG_IRE2, local_i0));
        return 1;
    }

    return 0;
}

static void wasm_emit_case(WasmBuf *b, WasmFuncIR *f, WasmOp *op,
                           int case_index, int loop_depth,
                           int local_pc, int local_fp, int local_cmp, int local_carry,
                           int local_i0, int local_f0, int local_tmp64)
{
    int next_index = case_index + 1;
    int dst, src, target_index;

    if (next_index > f->nb_ops)
        next_index = f->nb_ops;

    switch (op->kind) {
    case WASM_OP_I32_CONST:
        dst = wasm_i32_reg_local(op->r0, local_i0);
        wb_i32_const(b, op->imm);
        wb_local_set(b, dst);
        break;

    case WASM_OP_F64_CONST:
        dst = wasm_f64_reg_local(op->r0, local_f0);
        wb_f64_const(b, op->f64);
        wb_local_set(b, dst);
        break;

    case WASM_OP_MOV_I32:
        dst = wasm_i32_reg_local(op->r0, local_i0);
        src = wasm_i32_reg_local(op->r1, local_i0);
        wb_local_get(b, src);
        wb_local_set(b, dst);
        break;

    case WASM_OP_MOV_F64:
        dst = wasm_f64_reg_local(op->r0, local_f0);
        src = wasm_f64_reg_local(op->r1, local_f0);
        wb_local_get(b, src);
        wb_local_set(b, dst);
        break;

    case WASM_OP_ADDR_LOCAL:
        dst = wasm_i32_reg_local(op->r0, local_i0);
        wb_local_get(b, local_fp);
        if (op->imm)
            wb_i32_const(b, op->imm), wb_u8(b, 0x6a);
        wb_local_set(b, dst);
        break;

    case WASM_OP_ADDR_SYM:
        dst = wasm_i32_reg_local(op->r0, local_i0);
        wb_i32_const(b, wasm_sym_addr_from_op(op));
        wb_local_set(b, dst);
        break;

    case WASM_OP_LOAD_I32:
    case WASM_OP_LOAD_S8:
    case WASM_OP_LOAD_U8:
    case WASM_OP_LOAD_S16:
    case WASM_OP_LOAD_U16:
        dst = wasm_i32_reg_local(op->r0, local_i0);
        wasm_emit_addr(b, op, local_fp, local_i0);
        switch (op->kind) {
        case WASM_OP_LOAD_I32: wb_u8(b, 0x28), wb_memarg(b, 2); break;
        case WASM_OP_LOAD_S8: wb_u8(b, 0x2c), wb_memarg(b, 0); break;
        case WASM_OP_LOAD_U8: wb_u8(b, 0x2d), wb_memarg(b, 0); break;
        case WASM_OP_LOAD_S16: wb_u8(b, 0x2e), wb_memarg(b, 1); break;
        default: wb_u8(b, 0x2f), wb_memarg(b, 1); break;
        }
        wb_local_set(b, dst);
        break;

    case WASM_OP_LOAD_F32:
        dst = wasm_f64_reg_local(op->r0, local_f0);
        wasm_emit_addr(b, op, local_fp, local_i0);
        wb_u8(b, 0x2a), wb_memarg(b, 2);
        wb_u8(b, 0xbb); /* f64.promote_f32 */
        wb_local_set(b, dst);
        break;

    case WASM_OP_LOAD_F64:
        dst = wasm_f64_reg_local(op->r0, local_f0);
        wasm_emit_addr(b, op, local_fp, local_i0);
        wb_u8(b, 0x2b), wb_memarg(b, 3);
        wb_local_set(b, dst);
        break;

    case WASM_OP_STORE_I32:
    case WASM_OP_STORE_I64:
    case WASM_OP_STORE_I8:
    case WASM_OP_STORE_I16:
        src = wasm_i32_reg_local(op->r0, local_i0);
        wasm_emit_addr(b, op, local_fp, local_i0);
        if (op->kind == WASM_OP_STORE_I64) {
            wasm_push_i64_from_i32_pair(b, src, wasm_i32_reg_local(op->r2, local_i0));
            wb_u8(b, 0x37), wb_memarg(b, 3);
        } else {
            wb_local_get(b, src);
            if (op->kind == WASM_OP_STORE_I32)
                wb_u8(b, 0x36), wb_memarg(b, 2);
            else if (op->kind == WASM_OP_STORE_I8)
                wb_u8(b, 0x3a), wb_memarg(b, 0);
            else
                wb_u8(b, 0x3b), wb_memarg(b, 1);
        }
        break;

    case WASM_OP_STORE_F32:
        src = wasm_f64_reg_local(op->r0, local_f0);
        wasm_emit_addr(b, op, local_fp, local_i0);
        wb_local_get(b, src);
        wb_u8(b, 0xb6); /* f32.demote_f64 */
        wb_u8(b, 0x38), wb_memarg(b, 2);
        break;

    case WASM_OP_STORE_F64:
        src = wasm_f64_reg_local(op->r0, local_f0);
        wasm_emit_addr(b, op, local_fp, local_i0);
        wb_local_get(b, src);
        wb_u8(b, 0x39), wb_memarg(b, 3);
        break;

    case WASM_OP_I32_BIN:
        dst = wasm_i32_reg_local(op->r0, local_i0);
        wb_local_get(b, dst);
        if (op->flags & WASM_OP_FLAG_IMM)
            wb_i32_const(b, op->imm);
        else
            wb_local_get(b, wasm_i32_reg_local(op->r1, local_i0));
        wb_u8(b, wasm_i32_bin_opcode(op->op));
        wb_local_set(b, dst);
        break;

    case WASM_OP_I32_ADDC:
    case WASM_OP_I32_SUBC:
        dst = wasm_i32_reg_local(op->r0, local_i0);
        wb_local_get(b, dst);
        wb_u8(b, 0xad); /* i64.extend_i32_u */
        if (op->flags & WASM_OP_FLAG_IMM)
            wb_i32_const(b, op->imm);
        else
            wb_local_get(b, wasm_i32_reg_local(op->r1, local_i0));
        wb_u8(b, 0xad); /* i64.extend_i32_u */
        wb_u8(b, op->kind == WASM_OP_I32_ADDC ? 0x7c : 0x7d); /* i64.add/sub */
        if (op->flags & WASM_OP_FLAG_INVERT) {
            wb_local_get(b, local_carry);
            wb_u8(b, 0xad); /* i64.extend_i32_u */
            wb_u8(b, op->kind == WASM_OP_I32_ADDC ? 0x7c : 0x7d); /* i64.add/sub */
        }
        wasm_store_i64_to_i32_pair(b, local_tmp64, dst, local_carry);
        wb_local_get(b, local_carry);
        wb_i32_const(b, 1);
        wb_u8(b, 0x71); /* i32.and */
        wb_local_set(b, local_carry);
        break;

    case WASM_OP_UMULL_U32:
        dst = wasm_i32_reg_local(op->r0, local_i0);
        wb_local_get(b, dst);
        wb_u8(b, 0xad); /* i64.extend_i32_u */
        if (op->flags & WASM_OP_FLAG_IMM)
            wb_i32_const(b, op->imm);
        else
            wb_local_get(b, wasm_i32_reg_local(op->r1, local_i0));
        wb_u8(b, 0xad); /* i64.extend_i32_u */
        wb_u8(b, 0x7e); /* i64.mul */
        wasm_store_i64_to_i32_pair(b, local_tmp64, dst, wasm_i32_reg_local(op->r2, local_i0));
        break;

    case WASM_OP_I32_NEG:
        dst = wasm_i32_reg_local(op->r0, local_i0);
        wb_i32_const(b, 0);
        wb_local_get(b, dst);
        wb_u8(b, 0x6b);
        wb_local_set(b, dst);
        break;

    case WASM_OP_F64_BIN:
        dst = wasm_f64_reg_local(op->r0, local_f0);
        src = wasm_f64_reg_local(op->r1, local_f0);
        wb_local_get(b, dst);
        wb_local_get(b, src);
        wb_u8(b, wasm_f_bin_opcode(op->op, 0));
        wb_local_set(b, dst);
        break;

    case WASM_OP_F32_BIN:
        dst = wasm_f64_reg_local(op->r0, local_f0);
        src = wasm_f64_reg_local(op->r1, local_f0);
        wb_local_get(b, dst), wb_u8(b, 0xb6);
        wb_local_get(b, src), wb_u8(b, 0xb6);
        wb_u8(b, wasm_f_bin_opcode(op->op, 1));
        wb_u8(b, 0xbb);
        wb_local_set(b, dst);
        break;

    case WASM_OP_F64_NEG:
        dst = wasm_f64_reg_local(op->r0, local_f0);
        wb_local_get(b, dst);
        wb_u8(b, 0x9a);
        wb_local_set(b, dst);
        break;

    case WASM_OP_F32_NEG:
        dst = wasm_f64_reg_local(op->r0, local_f0);
        wb_local_get(b, dst), wb_u8(b, 0xb6);
        wb_u8(b, 0x8c);
        wb_u8(b, 0xbb);
        wb_local_set(b, dst);
        break;

    case WASM_OP_SET_CMP_I32:
        wb_local_get(b, wasm_i32_reg_local(op->r0, local_i0));
        if (op->flags & WASM_OP_FLAG_IMM)
            wb_i32_const(b, op->imm);
        else
            wb_local_get(b, wasm_i32_reg_local(op->r1, local_i0));
        wb_u8(b, wasm_i32_cmp_opcode(op->op));
        wb_local_set(b, local_cmp);
        break;

    case WASM_OP_SET_CMP_F32:
        wb_local_get(b, wasm_f64_reg_local(op->r0, local_f0)), wb_u8(b, 0xb6);
        wb_local_get(b, wasm_f64_reg_local(op->r1, local_f0)), wb_u8(b, 0xb6);
        wb_u8(b, wasm_f32_cmp_opcode(op->op));
        wb_local_set(b, local_cmp);
        break;

    case WASM_OP_SET_CMP_F64:
        wb_local_get(b, wasm_f64_reg_local(op->r0, local_f0));
        wb_local_get(b, wasm_f64_reg_local(op->r1, local_f0));
        wb_u8(b, wasm_f64_cmp_opcode(op->op));
        wb_local_set(b, local_cmp);
        break;

    case WASM_OP_SET_I32_FROM_CMP:
        dst = wasm_i32_reg_local(op->r0, local_i0);
        wb_local_get(b, local_cmp);
        if (op->flags & WASM_OP_FLAG_INVERT)
            wb_u8(b, 0x45); /* i32.eqz */
        wb_local_set(b, dst);
        break;

    case WASM_OP_JMP:
        target_index = wasm_pc_to_index(f, op->target_pc);
        wb_i32_const(b, target_index);
        wb_local_set(b, local_pc);
        wb_u8(b, 0x0c), wb_uleb(b, loop_depth);
        return;

    case WASM_OP_JMP_CMP:
        target_index = wasm_pc_to_index(f, op->target_pc);
        wb_local_get(b, local_cmp);
        if (op->flags & WASM_OP_FLAG_INVERT)
            wb_u8(b, 0x45);
        wb_u8(b, 0x04), wb_u8(b, 0x7f); /* if (result i32) */
        wb_i32_const(b, target_index);
        wb_u8(b, 0x05); /* else */
        wb_i32_const(b, next_index);
        wb_u8(b, 0x0b); /* end */
        wb_local_set(b, local_pc);
        wb_u8(b, 0x0c), wb_uleb(b, loop_depth);
        return;

    case WASM_OP_ITOF_F32:
        dst = wasm_f64_reg_local(op->r0, local_f0);
        wb_local_get(b, wasm_i32_reg_local(op->r1, local_i0));
        wb_u8(b, (op->flags & WASM_OP_FLAG_INVERT) ? 0xb3 : 0xb2);
        wb_u8(b, 0xbb);
        wb_local_set(b, dst);
        break;

    case WASM_OP_ITOF_F64:
        dst = wasm_f64_reg_local(op->r0, local_f0);
        wb_local_get(b, wasm_i32_reg_local(op->r1, local_i0));
        wb_u8(b, (op->flags & WASM_OP_FLAG_INVERT) ? 0xb8 : 0xb7);
        wb_local_set(b, dst);
        break;

    case WASM_OP_I64_TOF_F32:
        dst = wasm_f64_reg_local(op->r0, local_f0);
        wasm_push_i64_from_i32_pair(b,
            wasm_i32_reg_local(op->r1, local_i0),
            wasm_i32_reg_local(op->r2, local_i0));
        wb_u8(b, (op->flags & WASM_OP_FLAG_INVERT) ? 0xb5 : 0xb4);
        wb_u8(b, 0xbb);
        wb_local_set(b, dst);
        break;

    case WASM_OP_I64_TOF_F64:
        dst = wasm_f64_reg_local(op->r0, local_f0);
        wasm_push_i64_from_i32_pair(b,
            wasm_i32_reg_local(op->r1, local_i0),
            wasm_i32_reg_local(op->r2, local_i0));
        wb_u8(b, (op->flags & WASM_OP_FLAG_INVERT) ? 0xba : 0xb9);
        wb_local_set(b, dst);
        break;

    case WASM_OP_FTOI_I32:
        dst = wasm_i32_reg_local(op->r0, local_i0);
        wb_local_get(b, wasm_f64_reg_local(op->r1, local_f0));
        wb_u8(b, (op->flags & WASM_OP_FLAG_INVERT) ? 0xab : 0xa9);
        wb_local_set(b, dst);
        break;

    case WASM_OP_FTOI_I64:
        wb_local_get(b, wasm_f64_reg_local(op->r1, local_f0));
        wb_u8(b, (op->flags & WASM_OP_FLAG_INVERT) ? 0xb1 : 0xb0);
        wasm_store_i64_to_i32_pair(b, local_tmp64,
            wasm_i32_reg_local(op->r0, local_i0),
            wasm_i32_reg_local(op->r2, local_i0));
        break;

    case WASM_OP_FTOF_TO_F32:
        dst = wasm_f64_reg_local(op->r0, local_f0);
        wb_local_get(b, dst);
        wb_u8(b, 0xb6);
        wb_u8(b, 0xbb);
        wb_local_set(b, dst);
        break;

    case WASM_OP_CALL:
    case WASM_OP_CALL_INDIRECT:
    {
        int i, libcall_done = 0;
        if (op->kind == WASM_OP_CALL) {
            int fi = wasm_find_func_index_by_tok(op->call_tok);
            if (fi >= 0) {
                for (i = 0; i < op->call_nb_args; ++i)
                    wasm_emit_call_arg(b, op, i, local_fp, local_i0, local_f0);
                wb_u8(b, 0x10), wb_uleb(b, fi);
            } else {
                libcall_done = wasm_emit_libcall(b, op, local_fp, local_i0, local_f0, local_tmp64);
                if (!libcall_done) {
                    tcc_error("wasm32 backend: unresolved direct call '%s'",
                              get_tok_str(op->call_tok, NULL));
                }
            }
        } else {
            if (op->imm < 0)
                tcc_error("wasm32 backend: missing type index for indirect call");
            for (i = 0; i < op->call_nb_args; ++i)
                wasm_emit_call_arg(b, op, i, local_fp, local_i0, local_f0);
            wb_local_get(b, wasm_i32_reg_local(op->r0, local_i0));
            wb_u8(b, 0x11), wb_uleb(b, op->imm), wb_uleb(b, 0);
        }
        if (!libcall_done) {
            if (op->type == WASM_VAL_I32)
                wb_local_set(b, wasm_i32_reg_local(REG_IRET, local_i0));
            else if (op->type == WASM_VAL_I64)
                wasm_store_i64_to_i32_pair(b, local_tmp64,
                    wasm_i32_reg_local(REG_IRET, local_i0),
                    wasm_i32_reg_local(REG_IRE2, local_i0));
            else if (op->type == WASM_VAL_F32) {
                wb_u8(b, 0xbb);
                wb_local_set(b, wasm_f64_reg_local(REG_FRET, local_f0));
            } else if (op->type == WASM_VAL_F64)
                wb_local_set(b, wasm_f64_reg_local(REG_FRET, local_f0));
        }
        break;
    }

    case WASM_OP_RET:
        wb_i32_const(b, f->nb_ops);
        wb_local_set(b, local_pc);
        wb_u8(b, 0x0c), wb_uleb(b, loop_depth);
        return;

    default:
        tcc_error("wasm32 backend: unknown IR op kind %u", op->kind);
        break;
    }

    wb_i32_const(b, next_index);
    wb_local_set(b, local_pc);
    wb_u8(b, 0x0c), wb_uleb(b, loop_depth);
}

static void wasm_emit_function_body(WasmBuf *code, WasmFuncIR *f)
{
    WasmBuf body;
    int i;
    int local_pc, local_fp, local_cmp, local_carry, local_i0, local_f0, local_tmp64;

    memset(&body, 0, sizeof(body));

    /* local declarations: [8 x i32] [4 x f64] [1 x i64] */
    wb_uleb(&body, 3);
    wb_uleb(&body, 8), wb_u8(&body, 0x7f);
    wb_uleb(&body, 4), wb_u8(&body, 0x7c);
    wb_uleb(&body, 1), wb_u8(&body, 0x7e);

    local_pc = f->nb_params;
    local_fp = local_pc + 1;
    local_cmp = local_fp + 1;
    local_carry = local_cmp + 1;
    local_i0 = local_carry + 1;
    local_f0 = local_i0 + 4;
    local_tmp64 = local_f0 + 4;

    /* prolog: set fp and reserve stack */
    if (f->frame_size) {
        wb_global_get(&body, 0);
        wb_i32_const(&body, f->frame_size);
        wb_u8(&body, 0x6b); /* i32.sub */
        wb_local_tee(&body, local_fp);
        wb_global_set(&body, 0);
    } else {
        wb_global_get(&body, 0);
        wb_local_set(&body, local_fp);
    }

    wb_i32_const(&body, 0);
    wb_local_set(&body, local_cmp);
    wb_i32_const(&body, 0);
    wb_local_set(&body, local_carry);

    /* spill wasm params to linear-memory frame slots */
    for (i = 0; i < f->nb_params; ++i) {
        wb_local_get(&body, local_fp);
        if (f->param_offsets[i])
            wb_i32_const(&body, f->param_offsets[i]), wb_u8(&body, 0x6a);
        wb_local_get(&body, i);
        if (f->param_types[i] == WASM_VAL_I32)
            wb_u8(&body, 0x36), wb_memarg(&body, 2);
        else if (f->param_types[i] == WASM_VAL_I64)
            wb_u8(&body, 0x37), wb_memarg(&body, 3);
        else if (f->param_types[i] == WASM_VAL_F32)
            wb_u8(&body, 0x38), wb_memarg(&body, 2);
        else if (f->param_types[i] == WASM_VAL_F64)
            wb_u8(&body, 0x39), wb_memarg(&body, 3);
        else
            tcc_error("wasm32 backend: bad parameter type %d", f->param_types[i]);
    }

    if (f->nb_ops > 0) {
        wb_i32_const(&body, 0);
        wb_local_set(&body, local_pc);

        wb_u8(&body, 0x03), wb_u8(&body, 0x40); /* loop */
        wb_u8(&body, 0x02), wb_u8(&body, 0x40); /* halt block */

        for (i = 0; i < f->nb_ops; ++i)
            wb_u8(&body, 0x02), wb_u8(&body, 0x40);

        wb_local_get(&body, local_pc);
        wb_u8(&body, 0x0e);
        wb_uleb(&body, f->nb_ops);
        for (i = 0; i < f->nb_ops; ++i)
            wb_uleb(&body, f->nb_ops - 1 - i);
        wb_uleb(&body, f->nb_ops); /* default -> halt block */

        for (i = f->nb_ops - 1; i >= 0; --i) {
            wb_u8(&body, 0x0b); /* end block i */
            wasm_emit_case(&body, f, &f->ops[i], i, i + 1,
                           local_pc, local_fp, local_cmp, local_carry,
                           local_i0, local_f0, local_tmp64);
        }

        wb_u8(&body, 0x0b); /* end halt block */
        wb_u8(&body, 0x0b); /* end loop */
    }

    /* epilog: restore stack pointer */
    wb_local_get(&body, local_fp);
    if (f->frame_size)
        wb_i32_const(&body, f->frame_size), wb_u8(&body, 0x6a);
    wb_global_set(&body, 0);

    if (f->ret_type == WASM_VAL_I32)
        wb_local_get(&body, wasm_i32_reg_local(REG_IRET, local_i0));
    else if (f->ret_type == WASM_VAL_I64)
        wasm_push_i64_from_i32_pair(&body,
            wasm_i32_reg_local(REG_IRET, local_i0),
            wasm_i32_reg_local(REG_IRE2, local_i0));
    else if (f->ret_type == WASM_VAL_F32)
        wb_local_get(&body, wasm_f64_reg_local(REG_FRET, local_f0)), wb_u8(&body, 0xb6);
    else if (f->ret_type == WASM_VAL_F64)
        wb_local_get(&body, wasm_f64_reg_local(REG_FRET, local_f0));

    wb_u8(&body, 0x0b); /* end function body */

    wb_uleb(code, body.len);
    wb_mem(code, body.data, body.len);
    tcc_free(body.data);
}

static void wasm_add_section(WasmBuf *mod, int id, WasmBuf *sec)
{
    if (sec->len == 0)
        return;
    wb_u8(mod, id);
    wb_uleb(mod, sec->len);
    wb_mem(mod, sec->data, sec->len);
}

static void wasm_str(WasmBuf *b, const char *s)
{
    int n = (int)strlen(s);
    wb_uleb(b, n);
    wb_mem(b, s, n);
}

ST_FUNC int tcc_output_wasm(TCCState *s1, const char *filename)
{
    WasmBuf mod, sec_type, sec_func, sec_table, sec_mem, sec_glob, sec_exp, sec_elem, sec_code, sec_data;
    WasmSig *extra_sigs = NULL;
    int cap_extra_sigs = 0;
    int nb_extra_sigs = 0;
    int has_indirect_calls = 0;
    int fd, i, nb_exports;
    int ro_size, data_size, bss_size, stack_size, memory_pages;
    int cur;
    TCCState *old_state = tcc_state;

    memset(&mod, 0, sizeof(mod));
    memset(&sec_type, 0, sizeof(sec_type));
    memset(&sec_func, 0, sizeof(sec_func));
    memset(&sec_table, 0, sizeof(sec_table));
    memset(&sec_mem, 0, sizeof(sec_mem));
    memset(&sec_glob, 0, sizeof(sec_glob));
    memset(&sec_exp, 0, sizeof(sec_exp));
    memset(&sec_elem, 0, sizeof(sec_elem));
    memset(&sec_code, 0, sizeof(sec_code));
    memset(&sec_data, 0, sizeof(sec_data));

    tcc_state = s1;
    wasm_sec_text = text_section;
    wasm_sec_data = data_section;
    wasm_sec_rodata = rodata_section;
    wasm_sec_bss = bss_section;

    ro_size = wasm_sec_rodata ? (int)wasm_sec_rodata->data_offset : 0;
    data_size = wasm_sec_data ? (int)wasm_sec_data->data_offset : 0;
    bss_size = wasm_sec_bss ? (int)wasm_sec_bss->data_offset : 0;
    stack_size = 65536;

    cur = 1024;
    wasm_layout.rodata_base = ro_size ? wasm_align_up(cur, 16) : 0;
    cur = wasm_layout.rodata_base + ro_size;
    wasm_layout.data_base = data_size ? wasm_align_up(cur, 16) : 0;
    cur = wasm_layout.data_base + data_size;
    wasm_layout.bss_base = wasm_align_up(cur, 16);
    cur = wasm_layout.bss_base + bss_size;
    wasm_layout.stack_top = wasm_align_up(cur + stack_size, 16);
    memory_pages = (wasm_layout.stack_top + 65535) / 65536;

    /* Resolve static data initializers that require symbol addresses. */
    wasm_apply_data_relocs(wasm_sec_rodata);
    wasm_apply_data_relocs(wasm_sec_data);

    /* Resolve call_indirect signatures and assign type indices. */
    for (i = 0; i < tcc_wasm_nb_funcs; ++i) {
        WasmFuncIR *f = &tcc_wasm_funcs[i];
        int k;
        for (k = 0; k < f->nb_ops; ++k) {
            WasmOp *op = &f->ops[k];
            WasmSig wanted;
            int ti = -1, j;
            if (op->kind != WASM_OP_CALL_INDIRECT)
                continue;
            has_indirect_calls = 1;
            wanted.ret_type = op->type;
            wanted.nb_params = op->call_nb_args;
            memcpy(wanted.param_types, op->call_arg_type, op->call_nb_args);
            for (j = 0; j < tcc_wasm_nb_funcs; ++j) {
                WasmFuncIR *g = &tcc_wasm_funcs[j];
                if (g->nb_params <= WASM_MAX_CALL_ARGS && wasm_sig_matches_func(&wanted, g)) {
                    ti = j;
                    break;
                }
            }
            if (ti < 0) {
                for (j = 0; j < nb_extra_sigs; ++j) {
                    if (wasm_sig_matches_op(&extra_sigs[j], op)) {
                        ti = tcc_wasm_nb_funcs + j;
                        break;
                    }
                }
                if (ti < 0) {
                    if (nb_extra_sigs >= cap_extra_sigs) {
                        int n = cap_extra_sigs ? cap_extra_sigs * 2 : 8;
                        extra_sigs = tcc_realloc(extra_sigs, n * sizeof(*extra_sigs));
                        cap_extra_sigs = n;
                    }
                    extra_sigs[nb_extra_sigs] = wanted;
                    ti = tcc_wasm_nb_funcs + nb_extra_sigs;
                    nb_extra_sigs++;
                }
            }
            op->imm = ti;
        }
    }

    /* type section: one type per function + extra indirect signatures. */
    wb_uleb(&sec_type, tcc_wasm_nb_funcs + nb_extra_sigs);
    for (i = 0; i < tcc_wasm_nb_funcs; ++i) {
        WasmFuncIR *f = &tcc_wasm_funcs[i];
        int j;
        wb_u8(&sec_type, 0x60);
        wb_uleb(&sec_type, f->nb_params);
        for (j = 0; j < f->nb_params; ++j)
            wb_u8(&sec_type, wasm_valtype_byte(f->param_types[j]));
        if (f->ret_type == WASM_VAL_VOID)
            wb_uleb(&sec_type, 0);
        else
            wb_uleb(&sec_type, 1), wb_u8(&sec_type, wasm_valtype_byte(f->ret_type));
    }
    for (i = 0; i < nb_extra_sigs; ++i) {
        int j;
        wb_u8(&sec_type, 0x60);
        wb_uleb(&sec_type, extra_sigs[i].nb_params);
        for (j = 0; j < extra_sigs[i].nb_params; ++j)
            wb_u8(&sec_type, wasm_valtype_byte(extra_sigs[i].param_types[j]));
        if (extra_sigs[i].ret_type == WASM_VAL_VOID)
            wb_uleb(&sec_type, 0);
        else
            wb_uleb(&sec_type, 1), wb_u8(&sec_type, wasm_valtype_byte(extra_sigs[i].ret_type));
    }

    /* function section */
    wb_uleb(&sec_func, tcc_wasm_nb_funcs);
    for (i = 0; i < tcc_wasm_nb_funcs; ++i)
        wb_uleb(&sec_func, i);

    if (has_indirect_calls) {
        /* one funcref table, with slot 0 reserved as null */
        wb_uleb(&sec_table, 1);
        wb_u8(&sec_table, 0x70); /* funcref */
        wb_u8(&sec_table, 0x00); /* min only */
        wb_uleb(&sec_table, tcc_wasm_nb_funcs + 1);

        wb_uleb(&sec_elem, 1);
        wb_u8(&sec_elem, 0x00); /* active segment for table 0 */
        wb_i32_const(&sec_elem, 1);
        wb_u8(&sec_elem, 0x0b);
        wb_uleb(&sec_elem, tcc_wasm_nb_funcs);
        for (i = 0; i < tcc_wasm_nb_funcs; ++i)
            wb_uleb(&sec_elem, i);
    }

    /* memory section: one linear memory */
    wb_uleb(&sec_mem, 1);
    wb_u8(&sec_mem, 0x00); /* limits: min only */
    wb_uleb(&sec_mem, memory_pages);

    /* mutable global __stack_pointer */
    wb_uleb(&sec_glob, 1);
    wb_u8(&sec_glob, 0x7f); /* i32 */
    wb_u8(&sec_glob, 0x01); /* mutable */
    wb_i32_const(&sec_glob, wasm_layout.stack_top);
    wb_u8(&sec_glob, 0x0b);

    /* exports: memory + non-static functions */
    nb_exports = 1;
    for (i = 0; i < tcc_wasm_nb_funcs; ++i) {
        if (tcc_wasm_funcs[i].is_static)
            continue;
        if (!tcc_wasm_funcs[i].name || !*tcc_wasm_funcs[i].name)
            continue;
        nb_exports++;
    }
    wb_uleb(&sec_exp, nb_exports);

    wasm_str(&sec_exp, "memory");
    wb_u8(&sec_exp, 0x02), wb_uleb(&sec_exp, 0);

    for (i = 0; i < tcc_wasm_nb_funcs; ++i) {
        const char *name;
        if (tcc_wasm_funcs[i].is_static)
            continue;
        name = tcc_wasm_funcs[i].name;
        if (!name || !*name)
            continue;
        wasm_str(&sec_exp, name);
        wb_u8(&sec_exp, 0x00);
        wb_uleb(&sec_exp, i);
    }

    /* code section */
    wb_uleb(&sec_code, tcc_wasm_nb_funcs);
    for (i = 0; i < tcc_wasm_nb_funcs; ++i)
        wasm_emit_function_body(&sec_code, &tcc_wasm_funcs[i]);

    /* data section (rodata + data). bss remains zero-initialized */
    if (ro_size || data_size) {
        int segs = (ro_size ? 1 : 0) + (data_size ? 1 : 0);
        wb_uleb(&sec_data, segs);
        if (ro_size) {
            wb_u8(&sec_data, 0x00);
            wb_i32_const(&sec_data, wasm_layout.rodata_base);
            wb_u8(&sec_data, 0x0b);
            wb_uleb(&sec_data, ro_size);
            wb_mem(&sec_data, wasm_sec_rodata->data, ro_size);
        }
        if (data_size) {
            wb_u8(&sec_data, 0x00);
            wb_i32_const(&sec_data, wasm_layout.data_base);
            wb_u8(&sec_data, 0x0b);
            wb_uleb(&sec_data, data_size);
            wb_mem(&sec_data, wasm_sec_data->data, data_size);
        }
    }

    /* module header */
    wb_u8(&mod, 0x00), wb_u8(&mod, 0x61), wb_u8(&mod, 0x73), wb_u8(&mod, 0x6d);
    wb_u8(&mod, 0x01), wb_u8(&mod, 0x00), wb_u8(&mod, 0x00), wb_u8(&mod, 0x00);

    wasm_add_section(&mod, 1, &sec_type);
    wasm_add_section(&mod, 3, &sec_func);
    wasm_add_section(&mod, 4, &sec_table);
    wasm_add_section(&mod, 5, &sec_mem);
    wasm_add_section(&mod, 6, &sec_glob);
    wasm_add_section(&mod, 7, &sec_exp);
    wasm_add_section(&mod, 9, &sec_elem);
    wasm_add_section(&mod, 10, &sec_code);
    wasm_add_section(&mod, 11, &sec_data);

    unlink(filename);
    fd = open(filename, O_WRONLY | O_CREAT | O_TRUNC | O_BINARY, 0666);
    if (fd < 0) {
        tcc_state = old_state;
        return tcc_error_noabort("could not write '%s: %s'", filename, strerror(errno));
    }
    if (write(fd, mod.data, mod.len) != (ssize_t)mod.len) {
        close(fd);
        tcc_state = old_state;
        return tcc_error_noabort("could not write '%s: %s'", filename, strerror(errno));
    }
    close(fd);

    if (s1->verbose)
        printf("<- %s\n", filename);

    tcc_free(mod.data);
    tcc_free(sec_type.data);
    tcc_free(sec_func.data);
    tcc_free(sec_table.data);
    tcc_free(sec_mem.data);
    tcc_free(sec_glob.data);
    tcc_free(sec_exp.data);
    tcc_free(sec_elem.data);
    tcc_free(sec_code.data);
    tcc_free(sec_data.data);
    tcc_free(extra_sigs);

    tcc_state = old_state;
    return 0;
}
