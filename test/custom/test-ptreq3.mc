int max(bool *x, bool *y) {
    if(x == NULL || y == NULL)
        return -1;
    else if(*x == *y)
        return 1;
    else
        return 0;
}

int main() {
    bool *x = NULL;
    bool *y = NULL;
    bool a = true;
    y = &a;
    print(max(x, y));
    x = &a;
    print(max(x, y));
    return 0;
}
