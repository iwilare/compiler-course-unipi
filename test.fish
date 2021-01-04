#!/usr/bin/fish

rm -f **.o
rm -f **.bc
rm -f **.elf
gcc src/rt-support.c -c -o rt-support.o
for f in test/*.mc
    echo
    set name (string replace -r '.[^.]*$' '' "$f")
    echo $name
    ./microcc $name.mc -o $name.bc
    llc $name.bc -filetype=obj -o $name.o
    gcc -no-pie $name.o rt-support.o -o $name.elf
    diff (./$name.elf | psub) (cat $name.out | psub)
    if test $status -eq 0
    else
        break
    end
end
