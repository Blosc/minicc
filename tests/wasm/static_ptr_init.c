static const int ro_vals[3] = { 19, 21, 23 };
static const int *ro_ptr = ro_vals;

static int ret3(void)
{
    return 3;
}

static int (*fn_ptr)(void) = ret3;

int main(void)
{
    int err = 0;

    if (ro_ptr[0] != 19 || ro_ptr[1] != 21 || ro_ptr[2] != 23)
        err |= 1;
    if (fn_ptr() != 3)
        err |= 2;

    return err;
}
