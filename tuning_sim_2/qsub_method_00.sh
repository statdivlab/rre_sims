#!/bin/sh
for num in {1..20};
do
    qsub helper_00.sh $num
done
