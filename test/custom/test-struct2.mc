struct b {
    int a;
    int b;
}

struct a {
    int a;
    struct b b;
}

int main() {
    struct a f;
    f.a   = 1;
    f.b.a = 2;
    f.b.b = 3;
    print(f.a);
    print(f.b.a);
    print(f.b.b);
    return 0;
}
