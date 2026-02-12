/* Test: structured control flow reconstruction
 * Exercises the Stackifier path (no "jump into loop" patterns)
 * and verifies correct br/br_if/block/loop emission. */

/* Simple while loop — should use Stackifier path */
static int sum_while(int n)
{
    int s = 0;
    int i = 0;
    while (i < n) {
        s += i;
        i++;
    }
    return s;
}

/* Nested while loops — Stackifier path */
static int nested_while(int n, int m)
{
    int total = 0;
    int i = 0;
    while (i < n) {
        int j = 0;
        while (j < m) {
            total += (i + 1) * (j + 1);
            j++;
        }
        i++;
    }
    return total;
}

/* do-while loop — Stackifier path */
static int do_while_test(int n)
{
    int s = 0;
    int i = 1;
    do {
        s += i;
        i++;
    } while (i <= n);
    return s;
}

/* Diamond if/else — tests forward branch scope nesting */
static int diamond(int x)
{
    int r;
    if (x > 0)
        r = x * 2;
    else
        r = -x * 3;
    return r + 1;
}

/* Chain of if/else-if with early return — deep scope nesting */
static int chain(int x)
{
    if (x < -100) return -3;
    if (x < 0)    return -1;
    if (x == 0)   return 0;
    if (x < 100)  return 1;
    return 3;
}

/* While with break — tests forward branch out of loop */
static int while_break(int n)
{
    int s = 0;
    int i = 0;
    while (i < 100) {
        if (i >= n)
            break;
        s += i;
        i++;
    }
    return s;
}

/* Simple for loop — uses fallback (TCC "jump into loop" pattern) */
static int sum_for(int n)
{
    int s = 0;
    int i;
    for (i = 0; i < n; i++) {
        s += i;
    }
    return s;
}

int main(void)
{
    int err = 0;

    /* 1. while loop */
    if (sum_while(10) != 45)
        err |= 1;
    if (sum_while(0) != 0)
        err |= 1;

    /* 2. nested while */
    if (nested_while(3, 4) != 60)
        err |= 2;

    /* 3. do-while */
    if (do_while_test(5) != 15)
        err |= 4;

    /* 4. diamond if/else */
    if (diamond(5) != 11)
        err |= 8;
    if (diamond(-4) != 13)
        err |= 8;

    /* 5. if chain */
    if (chain(-200) != -3)
        err |= 16;
    if (chain(-50) != -1)
        err |= 16;
    if (chain(0) != 0)
        err |= 16;
    if (chain(50) != 1)
        err |= 16;
    if (chain(200) != 3)
        err |= 16;

    /* 6. while with break */
    if (while_break(5) != 10)
        err |= 32;

    /* 7. for loop (fallback path) should still produce correct results */
    if (sum_for(10) != 45)
        err |= 64;

    return err;
}
