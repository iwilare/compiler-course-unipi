int main() {
    int a[10];
    int i;

    for(i = 0; i < 10; i++)
        a[i] = 0;

    i = 0;

    // A fundamental test!
    // This is why a simple desugaring
    /*
        a += v;
          ->
        a = a + v;
    */
    // does not work.

    a[i++] += 10; // In this case, a++ would be incremented twice.

    print(a[0]);
    print(i);

    return 0;
}
