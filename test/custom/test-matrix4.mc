void print_m1(int a[3][4]) {
    int i;
    int j;
    for(i = 0; i<3; i++)
        for(j = 0; j<4; j++)
            print(++a[i][j]);
}

int main() {
    int m[3][4];

    int i;
    int j;
    for(i = 0; i<3; i++)
        for(j = 0; j<4; j++)
            m[i][j] = 4*i + j;

    print_m1(m);
    print_m1(m);

    return 0;
}
