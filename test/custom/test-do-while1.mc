int main() {
    int i;

    i = 0;
    do {
        print(i);
    } while(false);

    i = 1;
    do {
        print(i++);
    } while(i < 10);

    return 0;
}
