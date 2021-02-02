int main() {
    float a;

    a = 0.0;

    a += a + 10.;
    printfloat(a);

    a += a + 10.e+2;
    printfloat(a);

    a -= a + .5;
    printfloat(a);

    a *= a + 2.0;
    printfloat(a);

    a *= a + 2.2E-1;
    printfloat(a);

    a /= a + 2.;
    printfloat(a);

    return 0;
}
