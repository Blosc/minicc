/* Test: basic block coalescing — compute-heavy kernel
 * A simplified mandelbrot-style computation that exercises tight loops
 * with many arithmetic ops per iteration — the key scenario for
 * coalescing speedups. */

static int mandelbrot_pixel(double cx, double cy, int max_iter)
{
    double zx = 0.0, zy = 0.0;
    int iter = 0;
    while (iter < max_iter) {
        /* These ops should all coalesce into one block */
        double zx2 = zx * zx;
        double zy2 = zy * zy;
        if (zx2 + zy2 > 4.0)
            break;
        double new_zx = zx2 - zy2 + cx;
        zy = 2.0 * zx * zy + cy;
        zx = new_zx;
        iter++;
    }
    return iter;
}

int main(void)
{
    int err = 0;

    /* Point inside the set: (-0.5, 0) — should reach max_iter */
    if (mandelbrot_pixel(-0.5, 0.0, 100) != 100)
        err |= 1;

    /* Point outside the set: (2.0, 0) — should escape quickly */
    {
        int n = mandelbrot_pixel(2.0, 0.0, 100);
        if (n >= 100 || n < 1)
            err |= 2;
    }

    /* Point on boundary: (0.25, 0) — should take many iterations */
    {
        int n = mandelbrot_pixel(0.25, 0.0, 200);
        if (n < 50)
            err |= 4;
    }

    /* Known escape point: (1, 1) — escapes in a few iterations */
    {
        int n = mandelbrot_pixel(1.0, 1.0, 100);
        if (n > 10)
            err |= 8;
    }

    return err;
}
