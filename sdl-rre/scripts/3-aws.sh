#!/bin/sh

# copy the needed Rscript
aws s3 cp s3://sdl-rre/scripts/main-2-input-ccc-r-ncores-nsim.R .

# run the simulator script
Rscript main-2-input-ccc-r-ncores-nsim.R 500 10 8 12

## copy file back
aws s3 sync . s3://sdl-rre/output/ --exclude '*' --include "*.csv"
