#!/bin/sh

# copy the needed Rscript and grid values
aws s3 cp s3://sdl-rre/scripts/zinb-03-p-delta-r-nsim-seed.R .
aws s3 cp s3://sdl-rre/scripts/zinb_other_delta.txt .

# index the lines
LINE=$((AWS_BATCH_JOB_ARRAY_INDEX + 1))
PARAMS=$(sed -n ${LINE}p zinb_other_delta.txt)

# run the simulator script
Rscript zinb-03-p-delta-r-nsim-seed.R ${PARAMS}

## copy file back
aws s3 sync . s3://sdl-rre/output/ --exclude '*' --include "*.csv"
