#!/bin/sh
for num in {1..20};
do
    qsub helper_02.sh $num
done
