typedef unsigned int u32;

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

    return err;
}
