void pptr(int *a) {
    if(a != NULL)
        print(*a);
    else
        print(-1);
}

int main() {
    int *p1 = NULL;
    int *p2 = NULL;
    int a = 10;

    p1 = &a;
    p2 = &a;

    pptr(p1);
    pptr(p2);

    *p1 = 20;

    pptr(p1);
    pptr(p2);

    p1 = NULL;

    pptr(p1);
    pptr(p2);
}
