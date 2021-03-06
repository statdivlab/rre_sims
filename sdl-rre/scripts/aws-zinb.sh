#!/bin/sh

# copy the needed Rscript
aws s3 cp s3://sdl-rre/scripts/main-methods03-zinb.R .

# run the simulator script
Rscript main-methods03-zinb.R 8 12

## copy file back
aws s3 sync . s3://sdl-rre/output/ --exclude '*' --include "*.csv"
