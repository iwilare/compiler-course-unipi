#!/bin/sh

CC="gcc -no-pie"

$CC src/rt-support.c -c -o rt-support.o

for file in test/**/*.mc ; do
    name=${file%%.*}
    echo $name
    ./microcc $name.mc -o $name.bc
    if [ $? -ne 0 ]; then
        break
    fi
    llc $name.bc -filetype=obj -o $name.o
    $CC $name.o rt-support.o -o $name.elf
    $name.elf > out.txt
    diff out.txt $name.out
    if [ $? -ne 0 ]; then
        rm -f $name.elf
        rm -f $name.o
        rm -f out.txt
        break
    fi
    rm -f $name.elf
    rm -f $name.o
    rm -f out.txt
done

rm -f test/**/*.o
rm -f test/**/*.bc
rm -f test/**/*.elf

rm -f rt-support.o
