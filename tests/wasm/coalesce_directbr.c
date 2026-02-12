/*
 * Test direct br / br_if optimisation (Phase B2).
 *
 * Exercises:
 *   - JMP forward (becomes br <depth>)
 *   - JMP_CMP both-forward (becomes br_if <depth_taken> / br <depth_not_taken>)
 *   - JMP_CMP forward-taken, backward-not-taken (br_if <depth>; fallback dispatch)
 *   - JMP backward (must still go through loop dispatch)
 *   - Deeply nested forward jumps (depth > 1)
 *   - Single-block functions (halt depth = 0)
 */

/* ---- helpers ----------------------------------------------------------- */
static int g_acc;                       /* global accumulator */

/* Single-block function: only RET, no jumps */
static int identity(int x) { return x; }

/* Backward jump: simple loop (while) */
static int sum_to(int n)
{
    int s = 0;
    while (n > 0) { s += n; n--; }
    return s;
}

/* Forward JMP_CMP: if / else */
static int abs_val(int x)
{
    if (x < 0) return -x;
    return x;
}

/* Deeply nested forward jumps: short-circuit || chain */
static int any_nonzero(int a, int b, int c, int d, int e)
{
    if (a != 0 || b != 0 || c != 0 || d != 0 || e != 0)
        return 1;
    return 0;
}

/* Mixed forward / backward: loop with early break */
static int first_ge(const int *arr, int n, int thresh)
{
    int i;
    for (i = 0; i < n; i++) {
        if (arr[i] >= thresh)
            return arr[i];
    }
    return -1;
}

/* Cascaded if/else-if producing many forward jumps */
static int classify(int x)
{
    if (x < 0)   return -1;
    if (x == 0)  return 0;
    if (x < 10)  return 1;
    if (x < 100) return 2;
    return 3;
}

/* Nested loops with break + continue (backward + forward mix) */
static int nested_loop(void)
{
    int total = 0;
    int i, j;
    for (i = 0; i < 5; i++) {
        if (i == 3) continue;        /* forward jump */
        for (j = 0; j < 4; j++) {
            if (j == 2) break;       /* forward jump */
            total += i * 10 + j;
        }
    }
    return total;
}

/* ---- main -------------------------------------------------------------- */
int main(void)
{
    int err = 0;
    static int arr[] = { 3, 7, 1, 9, 5 };

    /* single-block function */
    if (identity(42) != 42)                         err |= 1;

    /* backward loop */
    if (sum_to(10) != 55)                           err |= 2;

    /* forward branch (if/else) */
    if (abs_val(-7) != 7 || abs_val(7) != 7)        err |= 4;

    /* deeply nested forward (short-circuit ||) */
    if (any_nonzero(0,0,0,0,0) != 0)                err |= 8;
    if (any_nonzero(0,0,0,0,1) != 1)                err |= 16;
    if (any_nonzero(1,0,0,0,0) != 1)                err |= 32;

    /* loop with early break (forward + backward) */
    if (first_ge(arr, 5, 8) != 9)                   err |= 64;
    if (first_ge(arr, 5, 99) != -1)                  err |= 128;

    /* cascaded if/else-if */
    if (classify(-5) != -1)                          err |= 256;
    if (classify(0)  !=  0)                          err |= 512;
    if (classify(5)  !=  1)                          err |= 1024;
    if (classify(50) !=  2)                          err |= 2048;
    if (classify(500)!=  3)                          err |= 4096;

    /* nested loop with break + continue */
    if (nested_loop() != 144)                        err |= 8192;

    return err;
}
