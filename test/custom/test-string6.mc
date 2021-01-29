int main() {
    int i;

    char s[14] = "hello world!\n";

    for(i = 0; s[i] != '\0'; i++)
        printchar(s[i]);

    return 0;
}
