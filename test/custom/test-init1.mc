struct A {
    int a;
    int b;
}

int c = 3;

struct A s;

int main() {
    int a = 1;
    int b = 2;
    s.a = 10;
    s.b = 20;
    print(a);
    print(b);
    print(c);
    print(s.a);
    print(s.b);
    a++;
    b++;
    c++;
    s.a++;
    s.b++;
    print(a);
    print(b);
    print(c);
    print(s.a);
    print(s.b);
    return 0;
}
