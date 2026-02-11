typedef long long i64;
typedef unsigned long long u64;

extern i64 __divdi3(i64, i64);
extern i64 __udivdi3(u64, u64);
extern i64 __moddi3(i64, i64);
extern i64 __umoddi3(u64, u64);
extern i64 __ashldi3(i64, int);
extern i64 __lshrdi3(u64, int);
extern i64 __ashrdi3(i64, int);
extern float __floatundisf(u64);
extern double __floatundidf(u64);
extern long double __floatundixf(u64);
extern i64 __fixunssfdi(float);
extern i64 __fixunsdfdi(double);
extern i64 __fixunsxfdi(long double);

static int nearf(float a, float b)
{
    float d = a - b;
    if (d < 0.0f)
        d = -d;
    return d < 0.5f;
}

static int neard(double a, double b)
{
    double d = a - b;
    if (d < 0.0)
        d = -d;
    return d < 0.5;
}

int main(void)
{
    int err = 0;
    u64 u = 12345ull;
    long double ld;

    if (__divdi3(100, 7) != 14)
        err |= 1;
    if ((u64)__udivdi3(100ull, 7ull) != 14ull)
        err |= 2;
    if (__moddi3(100, 7) != 2)
        err |= 4;
    if ((u64)__umoddi3(100ull, 7ull) != 2ull)
        err |= 8;
    if (__ashldi3(3, 5) != 96)
        err |= 16;
    if ((u64)__lshrdi3(0x8000000000000000ull, 63) != 1ull)
        err |= 32;
    if (__ashrdi3(-16, 2) != -4)
        err |= 64;

    if (!nearf(__floatundisf(u), 12345.0f))
        err |= 128;
    if (!neard(__floatundidf(u), 12345.0))
        err |= 256;

    ld = __floatundixf(u);
    if ((u64)__fixunssfdi((float)ld) != u)
        err |= 512;
    if ((u64)__fixunsdfdi((double)ld) != u)
        err |= 1024;
    if ((u64)__fixunsxfdi(ld) != u)
        err |= 2048;

    return err;
}
