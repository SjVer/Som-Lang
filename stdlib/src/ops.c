#include "types.h"

int abs(int);
double pow(double, double);

// arithmetic ops

som_Int som_add(som_Int x, som_Int y) {
    return x + y;
}

som_Int som_sub(som_Int x, som_Int y) {
    return x - y;
}

som_Int som_mul(som_Int x, som_Int y) {
    return x * y;
}

som_Int som_div(som_Int x, som_Int y) {
    return x / y;
}

som_Int som_pow(som_Int x, som_Int y) {
    return (som_Int)pow(x, y);
}

som_Int som_mod(som_Int x, som_Int y) {
    return x % y;
}

som_Int som_abs(som_Int x) {
    return abs(x);
}

som_Int som_neg(som_Int x) {
    return -x;
}

// logical ops

som_Bln som_and(som_Bln x, som_Bln y) {
    return x && y;
}

som_Bln som_xor(som_Bln x, som_Bln y) {
    return x != y;
}

som_Bln som_or(som_Bln x, som_Bln y) {
    return x || y;
}

som_Bln som_not(som_Bln x) {
    return !x;
}

// comparison ops

som_Bln som_eq(som_Int x, som_Int y) {
    return x == y;
}

som_Bln som_gr(som_Int x, som_Int y) {
    return x > y;
}

som_Bln som_ls(som_Int x, som_Int y) {
    return x < y;
}