void print_vec1(int a[]) {
    int i;
    for(i = 0; i<4; i++)
        print(++a[i]);
}

int main() {
    int m[3][4];

    int i;
    int j;
    for(i = 0; i<3; i++)
        for(j = 0; j<4; j++)
            m[i][j] = 4*i + j;

    print_vec1(m[2]);
    print_vec1(m[2]);

    return 0;
}
