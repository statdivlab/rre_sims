#!/bin/sh
for num in {61..80};
do
    qsub fixed_lam.sh $num
done
