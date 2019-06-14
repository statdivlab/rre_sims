#!/bin/sh
for num in {41..60};
do
    qsub fixed_lam.sh $num
done
