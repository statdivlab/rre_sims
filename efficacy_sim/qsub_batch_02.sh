#!/bin/sh
for num in {21..40};
do
    qsub fixed_lam.sh $num
done
