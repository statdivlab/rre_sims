#!/bin/sh
for num in {1..20};
do
    qsub helper_04.sh $num
done
