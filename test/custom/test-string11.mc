/* Note: this somewhat surprisingly compiles in C, hinting at the
   underlying pointer representation declared in the standard */

void prints(char s[32]) {
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
