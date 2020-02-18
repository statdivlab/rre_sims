sudo yum update
conda update -n base -c defaults conda
conda create --name myR
conda activate myR

conda install -c r r
conda install -c r r-tidyverse
conda install -c conda-forge r-devtools
conda install -c r r-stringr
conda install -c r r-magrittr
# conda install -c bioconda bioconductor-biocinstaller

# sudo yum install libXt
# sudo yum install git
# sudo yum install screen
# getgit@github.com:statdivlab/rre_sims.git
