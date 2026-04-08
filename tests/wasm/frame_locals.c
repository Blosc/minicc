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
    double ds = 98.5;
    double du = 54321.125;

    if (cast_double_to_int(ds) != 98)
        err |= 1;
    if (cast_double_to_uint(du) != 54321u)
        err |= 2;

    return err;
}
