#!/bin/sh

# index the lines
# PARAMS=$(sed -n ${LINE}p nb_biglambda_grid.txt)
PARAMS=$(sed -n 5p nb_biglambda_grid.txt)

# run the simulator script
echo "Rscript nb-03-biglambdagrid-input-c-r-nsim-ncores ${PARAMS}"
