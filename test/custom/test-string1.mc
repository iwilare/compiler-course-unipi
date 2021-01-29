char s[14] = "hello world!\n";

int main() {
    int i;

    for(i = 0; s[i] != '\0'; i++)
        printchar(s[i]);

    return 0;
}
