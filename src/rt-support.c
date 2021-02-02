#include <stdio.h>
#include <stdlib.h>

int getint() {
    int n;
    scanf("%d", &n);
    return n;
}

void print(int n) {
    printf("%d\n", n);
}

void printfloat(float f) {
    printf("%f\n", f);
}

void printchar(char c) {
    printf("%c", c);
}
