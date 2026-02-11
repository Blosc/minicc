#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "libtcc.h"

typedef struct ErrorLog {
    int count;
    char buf[8192];
} ErrorLog;

static void test_error_cb(void *opaque, const char *msg)
{
    ErrorLog *log = (ErrorLog *)opaque;
    size_t used;
    size_t avail;

    if (!log || !msg)
        return;
    log->count++;

    used = strlen(log->buf);
    if (used >= sizeof(log->buf) - 2)
        return;

    avail = sizeof(log->buf) - used - 1;
    strncat(log->buf, msg, avail);
    used = strlen(log->buf);
    if (used < sizeof(log->buf) - 1) {
        log->buf[used] = '\n';
        log->buf[used + 1] = '\0';
    }
}

static int has_text(const ErrorLog *log, const char *needle)
{
    return log && needle && strstr(log->buf, needle) != NULL;
}

static TCCState *new_state(ErrorLog *log)
{
    TCCState *s = tcc_new();
    if (!s)
        return NULL;
    tcc_set_error_func(s, log, test_error_cb);
    if (tcc_set_options(s, "-nostdlib -nostdinc") < 0) {
        tcc_delete(s);
        return NULL;
    }
    if (tcc_set_output_type(s, TCC_OUTPUT_MEMORY) < 0) {
        tcc_delete(s);
        return NULL;
    }
    return s;
}

static int call_int_fn(TCCState *s, const char *name, int arg, int *out)
{
    int (*fn)(int);

    fn = (int (*)(int))tcc_get_symbol(s, name);
    if (!fn)
        return -1;
    *out = fn(arg);
    return 0;
}

static int call_int0_fn(TCCState *s, const char *name, int *out)
{
    int (*fn)(void);

    fn = (int (*)(void))tcc_get_symbol(s, name);
    if (!fn)
        return -1;
    *out = fn();
    return 0;
}

static int host_add(int a, int b)
{
    return a + b;
}

static int test_state_lifecycle(void)
{
    TCCState *s1 = NULL;
    TCCState *s2 = NULL;
    int one = 0, two = 0;
    int rc = 1;
    ErrorLog e1 = {0};
    ErrorLog e2 = {0};

    s1 = new_state(&e1);
    s2 = new_state(&e2);
    if (!s1 || !s2)
        goto done;

    if (tcc_compile_string(s1, "int one(void){return 1;}") < 0)
        goto done;
    if (tcc_compile_string(s2, "int two(int x){return x*2;}") < 0)
        goto done;
    if (tcc_relocate(s1) < 0 || tcc_relocate(s2) < 0)
        goto done;

    if (!tcc_get_symbol(s1, "one"))
        goto done;
    if (tcc_get_symbol(s2, "one") != NULL)
        goto done;

    if (call_int_fn(s2, "two", 11, &two) < 0)
        goto done;
    if (two != 22)
        goto done;

    tcc_delete(s1);
    s1 = NULL;

    if (call_int_fn(s2, "two", 12, &two) < 0)
        goto done;
    if (two != 24)
        goto done;

    if (((int (*)(void))tcc_get_symbol(s2, "one")) != NULL)
        goto done;

    rc = 0;
done:
    if (s1)
        tcc_delete(s1);
    if (s2)
        tcc_delete(s2);
    (void)one;
    return rc;
}

static int test_error_callback_contract(void)
{
    TCCState *s = NULL;
    ErrorLog log = {0};
    int rc = 1;

    s = new_state(&log);
    if (!s)
        goto done;
    if (tcc_compile_string(s, "int broken( { return 0; }") >= 0)
        goto done;
    if (log.count <= 0)
        goto done;
    if (!has_text(&log, "error"))
        goto done;
    tcc_delete(s);
    s = NULL;

    memset(&log, 0, sizeof(log));
    s = new_state(&log);
    if (!s)
        goto done;
    if (tcc_compile_string(s, "extern int missing(int); int use(int x){ return missing(x); }") < 0)
        goto done;
    if (tcc_relocate(s) >= 0)
        goto done;
    if (log.count <= 0)
        goto done;
    if (!has_text(&log, "missing"))
        goto done;

    rc = 0;
done:
    if (s)
        tcc_delete(s);
    return rc;
}

static int test_compile_fail_no_crash(void)
{
    TCCState *s = NULL;
    ErrorLog log = {0};
    int rc = 1;

    s = new_state(&log);
    if (!s)
        goto done;
    if (tcc_compile_string(s, "int oops( { return 1; }") >= 0)
        goto done;
    if (log.count <= 0)
        goto done;
    tcc_delete(s);
    s = NULL;

    memset(&log, 0, sizeof(log));
    s = new_state(&log);
    if (!s)
        goto done;
    if (tcc_compile_string(s, "extern int ext(int); int f(int x){ return ext(x); }") < 0)
        goto done;
    if (tcc_relocate(s) >= 0)
        goto done;
    if (!has_text(&log, "ext"))
        goto done;
    tcc_delete(s);
    s = NULL;

    memset(&log, 0, sizeof(log));
    s = new_state(&log);
    if (!s)
        goto done;
    if (tcc_compile_string(s, "int dup(void){return 1;}") < 0)
        goto done;
    if (tcc_compile_string(s, "int dup(void){return 2;}") >= 0)
        goto done;
    if (log.count <= 0)
        goto done;
    if (!has_text(&log, "dup"))
        goto done;

    rc = 0;
done:
    if (s)
        tcc_delete(s);
    return rc;
}

static int test_symbol_resolution_order(void)
{
    static const char *src = "extern int host_add(int, int); int use(int x){ return host_add(x, 10); }";
    TCCState *s = NULL;
    ErrorLog log = {0};
    int out = 0;
    int rc = 1;

    s = new_state(&log);
    if (!s)
        goto done;
    if (tcc_add_symbol(s, "host_add", (const void *)host_add) < 0)
        goto done;
    if (tcc_compile_string(s, src) < 0)
        goto done;
    if (tcc_relocate(s) < 0)
        goto done;
    if (call_int_fn(s, "use", 7, &out) < 0 || out != 17)
        goto done;
    tcc_delete(s);
    s = NULL;

    memset(&log, 0, sizeof(log));
    s = new_state(&log);
    if (!s)
        goto done;
    if (tcc_compile_string(s, src) < 0)
        goto done;
    if (tcc_add_symbol(s, "host_add", (const void *)host_add) < 0)
        goto done;
    if (tcc_relocate(s) < 0)
        goto done;
    if (call_int_fn(s, "use", 5, &out) < 0 || out != 15)
        goto done;
    tcc_delete(s);
    s = NULL;

    memset(&log, 0, sizeof(log));
    s = new_state(&log);
    if (!s)
        goto done;
    if (tcc_compile_string(s, src) < 0)
        goto done;
    if (tcc_relocate(s) >= 0)
        goto done;
    if (!has_text(&log, "host_add"))
        goto done;

    rc = 0;
done:
    if (s)
        tcc_delete(s);
    return rc;
}

static int test_multi_unit_api_flow(void)
{
    TCCState *s = NULL;
    ErrorLog log = {0};
    int out = 0;
    int rc = 1;

    s = new_state(&log);
    if (!s)
        goto done;

    if (tcc_compile_string(s, "int inc(int x){ return x + 1; }") < 0)
        goto done;
    if (tcc_compile_string(s, "extern int inc(int); int entry(int x){ return inc(x) + 41; }") < 0)
        goto done;
    if (tcc_relocate(s) < 0)
        goto done;
    if (call_int_fn(s, "entry", 1, &out) < 0 || out != 43)
        goto done;

    rc = 0;
done:
    if (s)
        tcc_delete(s);
    return rc;
}

static int test_define_option_flow(void)
{
    TCCState *s = NULL;
    ErrorLog log = {0};
    int out = 0;
    int rc = 1;
    static const char *src =
        "#ifndef OPT_A\n"
        "#error OPT_A must exist\n"
        "#endif\n"
        "#ifndef API_C\n"
        "#error API_C must exist\n"
        "#endif\n"
        "#ifdef OPT_B\n"
        "int cfg(void){ return -1000; }\n"
        "#else\n"
        "int cfg(void){ return OPT_A + API_C; }\n"
        "#endif\n";

    s = new_state(&log);
    if (!s)
        goto done;
    if (tcc_set_options(s, "-DOPT_A=9 -DOPT_B") < 0)
        goto done;
    tcc_define_symbol(s, "API_C", "11");
    tcc_undefine_symbol(s, "OPT_B");
    if (tcc_compile_string(s, src) < 0)
        goto done;
    if (tcc_relocate(s) < 0)
        goto done;
    if (call_int0_fn(s, "cfg", &out) < 0 || out != 20)
        goto done;

    rc = 0;
done:
    if (s)
        tcc_delete(s);
    return rc;
}

typedef struct SymbolSeen {
    int pub_seen;
    int hid_seen;
} SymbolSeen;

static void symbol_seen_cb(void *ctx, const char *name, const void *val)
{
    SymbolSeen *seen = (SymbolSeen *)ctx;
    (void)val;
    if (!seen || !name)
        return;
    if (strcmp(name, "pub") == 0)
        seen->pub_seen = 1;
    if (strcmp(name, "hid") == 0)
        seen->hid_seen = 1;
}

static int test_symbol_listing_contract(void)
{
    TCCState *s = NULL;
    ErrorLog log = {0};
    SymbolSeen seen = {0};
    int rc = 1;

    s = new_state(&log);
    if (!s)
        goto done;
    if (tcc_compile_string(s, "int pub(void){return 7;} static int hid(void){return 3;}") < 0)
        goto done;
    if (tcc_relocate(s) < 0)
        goto done;
    if (tcc_get_symbol(s, "pub") == NULL)
        goto done;
    if (tcc_get_symbol(s, "hid") != NULL)
        goto done;
    tcc_list_symbols(s, &seen, symbol_seen_cb);

    rc = 0;
done:
    if (s)
        tcc_delete(s);
    return rc;
}

static int test_state_macro_isolation(void)
{
    TCCState *s1 = NULL;
    TCCState *s2 = NULL;
    ErrorLog e1 = {0};
    ErrorLog e2 = {0};
    int v1 = 0, v2 = 0;
    int rc = 1;
    static const char *src = "int val(void){ return V; }";

    s1 = new_state(&e1);
    s2 = new_state(&e2);
    if (!s1 || !s2)
        goto done;

    tcc_define_symbol(s1, "V", "17");
    tcc_define_symbol(s2, "V", "29");
    if (tcc_compile_string(s1, src) < 0 || tcc_compile_string(s2, src) < 0)
        goto done;
    if (tcc_relocate(s1) < 0 || tcc_relocate(s2) < 0)
        goto done;
    if (call_int0_fn(s1, "val", &v1) < 0 || v1 != 17)
        goto done;
    if (call_int0_fn(s2, "val", &v2) < 0 || v2 != 29)
        goto done;

    rc = 0;
done:
    if (s1)
        tcc_delete(s1);
    if (s2)
        tcc_delete(s2);
    return rc;
}

int main(int argc, char **argv)
{
    const char *name;
    int rc;

    if (argc != 2) {
        fprintf(stderr, "usage: %s <case>\n", argv[0]);
        return 2;
    }

    name = argv[1];
    if (strcmp(name, "state_lifecycle") == 0)
        rc = test_state_lifecycle();
    else if (strcmp(name, "error_callback_contract") == 0)
        rc = test_error_callback_contract();
    else if (strcmp(name, "compile_fail_no_crash") == 0)
        rc = test_compile_fail_no_crash();
    else if (strcmp(name, "symbol_resolution_order") == 0)
        rc = test_symbol_resolution_order();
    else if (strcmp(name, "multi_unit_api_flow") == 0)
        rc = test_multi_unit_api_flow();
    else if (strcmp(name, "define_option_flow") == 0)
        rc = test_define_option_flow();
    else if (strcmp(name, "symbol_listing_contract") == 0)
        rc = test_symbol_listing_contract();
    else if (strcmp(name, "state_macro_isolation") == 0)
        rc = test_state_macro_isolation();
    else {
        fprintf(stderr, "unknown case: %s\n", name);
        return 2;
    }

    if (rc != 0)
        fprintf(stderr, "libtcc-api-test case failed: %s\n", name);
    return rc;
}
