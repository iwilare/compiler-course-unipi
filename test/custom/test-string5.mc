char g[14] = "hello world!\n";

void f(char s[]) {
    int i;

    for(i = 0; s[i] != '\n'; i++)
        s[i] = 'a';
}

void printstr(char s[]) {
    int i;
    for(i = 0; s[i] != '\0'; i++)
        printchar(s[i]);
}

int main() {
    int i;

    printstr(g);

    f(g);

    printstr(g);

    return 0;
}
