void prints(char s[14]) {
    int i;
    for(i = 0; s[i] != '\0'; i++)
        printchar(s[i]);
}

int main() {
    int i;

    char s[14] = "hello world!\n";

    prints(s);

    return 0;
}
