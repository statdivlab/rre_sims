#!/bin/sh
for num in {1..20};
do
    qsub helper_03.sh $num
done
