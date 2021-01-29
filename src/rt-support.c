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
    printf("%.3f\n", f); // With just %f it might be platform-dependent
}

void printchar(char c) {
    printf("%c", c);
}
