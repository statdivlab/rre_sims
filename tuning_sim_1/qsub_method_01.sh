#!/bin/sh
for num in {1..20};
do
    qsub helper_01.sh $num
done
