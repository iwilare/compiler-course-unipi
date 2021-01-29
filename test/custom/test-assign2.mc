int main() {
    float a;

    a = 0.0;

    a += a + 10.0;
    printfloat(a);

    a += a + 10.0;
    printfloat(a);

    a -= a + 10.0;
    printfloat(a);

    a *= a + 2.0;
    printfloat(a);

    a /= a + 2.0;
    printfloat(a);

    return 0;
}
