/* Test: basic block coalescing — loops and branches
 * Loops create multiple basic blocks (condition, body, increment).
 * This tests that block boundaries are correctly identified at
 * jump targets and after jumps. */

int main(void)
{
    int err = 0;

    /* 1. Simple for loop — sum 1..10 */
    {
        int sum = 0;
        int i;
        for (i = 1; i <= 10; i++)
            sum += i;
        if (sum != 55)
            err |= 1;
    }

    /* 2. Nested loops */
    {
        int total = 0;
        int i, j;
        for (i = 0; i < 5; i++)
            for (j = 0; j < 3; j++)
                total++;
        if (total != 15)
            err |= 2;
    }

    /* 3. While loop with early break */
    {
        int count = 0;
        int k = 0;
        while (k < 100) {
            count++;
            k += 7;
            if (k > 30)
                break;
        }
        /* k goes: 7, 14, 21, 28, 35 -> break at 35, count=5 */
        if (count != 5)
            err |= 4;
    }

    /* 4. Do-while loop */
    {
        int val = 1;
        int iter = 0;
        do {
            val *= 2;
            iter++;
        } while (val < 64);
        /* val: 2, 4, 8, 16, 32, 64 -> iter=6, val=64 */
        if (val != 64 || iter != 6)
            err |= 8;
    }

    /* 5. Loop with continue */
    {
        int sum = 0;
        int i;
        for (i = 0; i < 10; i++) {
            if (i % 2 == 0)
                continue;
            sum += i;
        }
        /* sum = 1+3+5+7+9 = 25 */
        if (sum != 25)
            err |= 16;
    }

    return err;
}
