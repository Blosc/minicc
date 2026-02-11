typedef long long i64;

static int add2(int a, int b)
{
    return a + b;
}

static i64 mul_add(i64 x, int y)
{
    return x * (i64)y + 7;
}

static double mix(double a, double b)
{
    return a * 2.0 + b;
}

static int call_i(int (*fn)(int, int), int x, int y)
{
    return fn(x, y);
}

static i64 call_ll(i64 (*fn)(i64, int), i64 x, int y)
{
    return fn(x, y);
}

static double call_d(double (*fn)(double, double), double x, double y)
{
    return fn(x, y);
}

int main(void)
{
    int err = 0;
    int (*pi)(int, int) = add2;
    i64 (*pll)(i64, int) = mul_add;
    double (*pd)(double, double) = mix;
    double v;

    if (call_i(pi, 4, 5) != 9)
        err |= 1;
    if (call_ll(pll, 10, 4) != 47)
        err |= 2;

    v = call_d(pd, 2.5, 1.25);
    if (v < 6.24 || v > 6.26)
        err |= 4;

    if (call_i((err & 1) ? add2 : pi, 7, 8) != 15)
        err |= 8;

    return err;
}
