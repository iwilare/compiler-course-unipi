struct A {
    int v[3][4];
}

void f(struct A s) {
    s.v[2][2] = 999;
}

void g(struct A* s) {
    (*s).v[2][2] = 999;
}

void print_m1(int a[3][4]) {
    print(a[2][2]);
}

int main() {
    struct A a;
    int i;
    int j;
    for(i = 0; i < 3; i++)
        for(j = 0; j < 4; j++)
            a.v[i][j] = 4*i + j;

    print_m1(a.v);

    f(a);

    print_m1(a.v);

    g(&a);

    print_m1(a.v);

    return 0;
}
