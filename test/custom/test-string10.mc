void prints(char s[]) {
    int i;
    for(i = 0; s[i] != '\0'; i++)
        printchar(s[i]);
}

int main() {
    int i;

    char s[] = "hello world!\n";

    prints(s);

    return 0;
}
