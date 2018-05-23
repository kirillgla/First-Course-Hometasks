/**
 * Based on algorithm described at
 * http://www.maths.surrey.ac.uk/hosted-sites/R.Knott/Fibonacci/cfINTRO.html
 * (Point 6.2.2)
 */

#include <stdio.h>
#include <ctype.h>
#include <math.h>

int sqr(int x)
{
    return x * x;
}

int readInput()
{
    printf("Please, enter a number: ");
    while (1)
    {
        int input;
        char after; // Used to ensure that input is integer.
        int result = scanf("%d%c", &input, &after);
        if (result == 2 && input > 0 && (int) sqrt(input) != sqrt(input) && isspace(after))
        {
            if(after != '\n')
            {
                while (getchar() != '\n'); // We need to free the input stream
            }
            return input;
        }
        else if (result != 2 || !isspace(after))
        {
            while (getchar() != '\n'); // We need to free the input stream
        }
        printf("Error! Please, enter a valid number: ");
    }
}

// Conventions:
// - n - input number;
// - m[i] - i-th digit of continued fraction;
// - x[i] - such number that x[0] = sqrt(n),
//                       and x[i-1] = m[i] + 1/x[i];
// x[i] can always be represented as (sqrt(n) + k[i]) / d[i],
// where k[i] and d[i] are integers.
// x[i] is never explicitly stored in this program.

int main()
{
    printf("Prints continued fraction representing square root of entered non-square number.\n");
    int n = readInput();
    int m = (int) sqrt(n); // We are looking forward to get x[i] = sqrt(n) + m
    printf("[%d; ", m);
    // Now sqrt(n) = m + 1/x
    // Hence, x = 1/(sqrt(n) - m) = (sqrt(n) + m) / (n - sqr(m))
    int d_i = n - sqr(m); // d[i] stands for "i-th denominator". This one is 0-th.
    if (d_i == 1)
    {
        printf("%d]\nPeriod = 1.", 2 * m);
        // That's what you'd get if you entered the following loop
        return 0;
    }

    int k_i = m; // i = 0
    for (int i = 1; ; i++)
    {
        // By this moment, k[i] and d[i] actually store values of k[i - 1] and d[i - 1]
        int d_i_minus_1 = d_i;
        int k_i_minus_1 = k_i;
        int m_i = (int) ((sqrt(n) + k_i_minus_1) / d_i_minus_1);
        printf("%d, ", m_i);

        // x[i - 1] = m[i] + 1/x[i]
        // Expressing x[i] in terms of x[i - 1], expanding x[i - 1] and simplifying results,
        // x[i] = d[i - 1] * (sqrt(n) - k[i-1] + d[i - 1] * m[i]) / (n - sqr(k[i - 1] - d[i - 1] * m[i]))
        // Looks just terrible, huh?

        k_i = -k_i_minus_1 + d_i_minus_1 * m_i;
        d_i = (n - sqr(k_i)) / d_i_minus_1;
        // This is integer for any i.

        if (d_i == 1 && k_i == m)
        {
            printf("%d]\nPeriod = %d.", 2 * m, i + 1);
            // That's what you'd get if you stayed in the loop
            return 0;
        }
    }
}
