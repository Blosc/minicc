/* Test: basic block coalescing — conditionals and multi-way branches
 * Tests if/else chains, ternary expressions, and nested conditionals
 * to exercise the block boundary detection at JMP_CMP targets. */

static int abs_val(int x)
{
    if (x < 0)
        return -x;
    return x;
}

static int clamp(int x, int lo, int hi)
{
    if (x < lo)
        return lo;
    if (x > hi)
        return hi;
    return x;
}

static int classify(int x)
{
    /* Multi-branch: chain of if/else if */
    if (x < 0)
        return -1;
    else if (x == 0)
        return 0;
    else if (x < 10)
        return 1;
    else if (x < 100)
        return 2;
    else
        return 3;
}

static int ternary_chain(int a, int b)
{
    /* Ternary expressions generate JMP/JMP_CMP patterns */
    int max = (a > b) ? a : b;
    int min = (a < b) ? a : b;
    return max - min;
}

int main(void)
{
    int err = 0;

    /* 1. Simple if/else */
    if (abs_val(-42) != 42)
        err |= 1;
    if (abs_val(17) != 17)
        err |= 1;

    /* 2. Multi-way clamp */
    if (clamp(5, 0, 10) != 5)
        err |= 2;
    if (clamp(-3, 0, 10) != 0)
        err |= 2;
    if (clamp(15, 0, 10) != 10)
        err |= 2;

    /* 3. If/else-if chain */
    if (classify(-5) != -1)
        err |= 4;
    if (classify(0) != 0)
        err |= 4;
    if (classify(7) != 1)
        err |= 4;
    if (classify(50) != 2)
        err |= 4;
    if (classify(200) != 3)
        err |= 4;

    /* 4. Ternary */
    if (ternary_chain(10, 3) != 7)
        err |= 8;
    if (ternary_chain(3, 10) != 7)
        err |= 8;

    /* 5. Nested conditionals */
    {
        int a = 5, b = 10, c = 3;
        int result;
        if (a > b) {
            if (a > c) result = a;
            else result = c;
        } else {
            if (b > c) result = b;
            else result = c;
        }
        if (result != 10)
            err |= 16;
    }

    return err;
}
