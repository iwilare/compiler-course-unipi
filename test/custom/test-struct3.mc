struct b {
    int a;
    int b;
}

struct a {
    int a;
    struct b b;
}

int f(struct a* s) {
    (*s).a = 1;
    return (*s).b.a;
}

void g(struct a s) {
    s.a = 999;
    s.b.a = 999;
    s.b.b = 999;
}

int main() {
    struct a s;
    s.a   = 10;
    s.b.a = 2;
    s.b.b = 3;
    g(s);
    f(&s);
    print(s.a);
    print(f(&s));
    print(s.b.b);
    return 0;
}
