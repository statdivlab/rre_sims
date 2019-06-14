#!/bin/sh
for num in {1..20};
do
    qsub fixed_lam.sh $num
done
