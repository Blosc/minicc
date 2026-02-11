static const int ro_vals[3] = { 19, 21, 23 };
static int data_val = 7;
static int bss_buf[4];

static int bump(void)
{
    data_val += 5;
    return data_val;
}

int main(void)
{
    int err = 0;

    if (ro_vals[0] != 19 || ro_vals[1] != 21 || ro_vals[2] != 23)
        err |= 1;

    if (data_val != 7)
        err |= 2;

    if (bss_buf[0] != 0 || bss_buf[3] != 0)
        err |= 4;

    bss_buf[2] = 11;
    if (bss_buf[2] != 11)
        err |= 8;

    if (bump() != 12)
        err |= 16;

    return err;
}
