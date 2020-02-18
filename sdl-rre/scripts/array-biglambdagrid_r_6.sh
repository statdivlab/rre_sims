#!/bin/sh

# copy the needed Rscript and grid values
aws s3 cp s3://sdl-rre/scripts/nb-03-biglambdagrid-input-c-r-nsim-ncores.R .
aws s3 cp s3://sdl-rre/scripts/nb_biglambda_grid_r_6.txt .

# index the lines
LINE=$((AWS_BATCH_JOB_ARRAY_INDEX + 1))
PARAMS=$(sed -n ${LINE}p nb_biglambda_grid_r_6.txt)

# run the simulator script
Rscript nb-03-biglambdagrid-input-c-r-nsim-ncores.R ${PARAMS}

## copy file back
aws s3 sync . s3://sdl-rre/output/ --exclude '*' --include "*.csv"
