typedef unsigned int u32;

static int cast_double_to_int(double x)
{
    return (int)x;
}

static unsigned cast_double_to_uint(double x)
{
    return (unsigned)x;
}

int main(void)
{
    int err = 0;
    float fs = -12.75f;
    float fu = 12345.875f;
    double ds = 98.5;
    double du = 54321.125;

    if ((int)fs != -12)
        err |= 1;

    if ((u32)fu != 12345u)
        err |= 2;

    if ((int)ds != 98)
        err |= 4;

    if ((u32)du != 54321u)
        err |= 8;

    if ((int)(fs + 2.25f) != -10)
        err |= 16;

    if ((u32)(du - 21.0) != 54300u)
        err |= 32;

    /* Exercise helper-call paths with caller locals kept live across calls. */
    if (cast_double_to_int(ds) != 98)
        err |= 64;

    if (cast_double_to_uint(du) != 54321u)
        err |= 128;

    return err;
}
