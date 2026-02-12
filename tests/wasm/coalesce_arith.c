/* Test: basic block coalescing — straight-line arithmetic
 * Multiple consecutive non-branching ops should be merged into a
 * single br_table case.  This tests that straight-line arithmetic
 * produces correct results after coalescing. */

int main(void)
{
    int err = 0;

    /* 1. Long chain of integer ops (should coalesce into one block) */
    int a = 10;
    int b = 20;
    int c = a + b;       /* 30 */
    int d = c * 3;       /* 90 */
    int e = d - a;       /* 80 */
    int f = e / 4;       /* 20 */
    int g = f + b + c;   /* 70 */
    if (g != 70)
        err |= 1;

    /* 2. Floating-point chain (should coalesce into one block) */
    double x = 2.5;
    double y = 3.0;
    double z = x * y;          /* 7.5 */
    double w = z + 1.5;        /* 9.0 */
    double v = w * 2.0 - 1.0;  /* 17.0 */
    if (v < 16.99 || v > 17.01)
        err |= 2;

    /* 3. Mixed int/float chain */
    int n = 7;
    double r = (double)n * 1.5;  /* 10.5 */
    if (r < 10.49 || r > 10.51)
        err |= 4;

    /* 4. Chained stores and loads */
    int arr[4];
    arr[0] = 100;
    arr[1] = arr[0] + 1;  /* 101 */
    arr[2] = arr[1] + 1;  /* 102 */
    arr[3] = arr[2] + 1;  /* 103 */
    if (arr[3] != 103)
        err |= 8;

    return err;
}
