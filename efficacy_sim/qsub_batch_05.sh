#!/bin/sh
for num in {81..100};
do
    qsub fixed_lam.sh $num
done
