/* Test: peephole optimization — chained register usage
 * Exercises sequences where the output register of one op is the
 * input register of the next, which should trigger local.tee/skip
 * optimization to reduce local.get/local.set pairs. */

static double dot_product(double ax, double ay, double bx, double by)
{
    /* Chain: mul -> mul -> add — all on f64 registers */
    return ax * bx + ay * by;
}

static int horner(int x, int a, int b, int c, int d)
{
    /* Horner's method: ((a*x + b)*x + c)*x + d
     * Each mul/add result feeds into the next op */
    return ((a * x + b) * x + c) * x + d;
}

static double lerp(double a, double b, double t)
{
    /* a + t * (b - a) — chained float ops */
    return a + t * (b - a);
}

int main(void)
{
    int err = 0;

    /* 1. Dot product */
    {
        double d = dot_product(3.0, 4.0, 1.0, 2.0);
        /* 3*1 + 4*2 = 11 */
        if (d < 10.99 || d > 11.01)
            err |= 1;
    }

    /* 2. Horner's method: 2x^3 + 3x^2 + 4x + 5 at x=2 */
    {
        int h = horner(2, 2, 3, 4, 5);
        /* ((2*2+3)*2+4)*2+5 = (7*2+4)*2+5 = 18*2+5 = 41 */
        if (h != 41)
            err |= 2;
    }

    /* 3. Linear interpolation */
    {
        double v = lerp(10.0, 20.0, 0.25);
        /* 10 + 0.25 * (20-10) = 12.5 */
        if (v < 12.49 || v > 12.51)
            err |= 4;
    }

    /* 4. Long chain of dependent integer ops */
    {
        int val = 1;
        val = val + 2;    /* 3 */
        val = val * 3;    /* 9 */
        val = val + 4;    /* 13 */
        val = val * 5;    /* 65 */
        val = val - 15;   /* 50 */
        if (val != 50)
            err |= 8;
    }

    /* 5. Comparison after arithmetic (set_cmp consumes arith result) */
    {
        int a = 10, b = 20;
        int c = a + b;   /* 30, result feeds into comparison */
        if (c > 50)
            err |= 16;
        if (c < 10)
            err |= 16;
    }

    return err;
}
