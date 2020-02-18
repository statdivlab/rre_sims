#!/bin/sh

# copy the needed Rscript
aws s3 cp s3://sdl-rre/scripts/main-input-ccc-r-ncores-nsim.R .

# run the simulator script
# eg
# Rscript main-input-ccc-r-ncores-nsim.R 500 6 8 12
Rscript main-input-ccc-r-ncores-nsim.R 500 6 2 2

## copy file back
aws s3 sync . s3://sdl-rre/output/ --exclude '*' --include "*.csv"
