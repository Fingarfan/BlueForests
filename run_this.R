rm(list = ls()) #Clearing the environment
gc()

#Dependencies ------------------------------------------------------------
#Install the version of bnlearn that works with this code
#This section should only be run once!
devtools::install_version("bnlearn", version = "4.9.1", repos = "http://cran.us.r-project.org")

if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install()
BiocManager::install(c("graph", "Rgraphviz", "RBGL"))
install.packages("gRain")
#This section on dependencies should only be run once!


setwd(dirname(rstudioapi::getSourceEditorContext()$path))

#Run each file one at a time, this way you can avoid issues of the code breaking down
source('BN_Discretize_Final.R')
source('Model_Estimation.R')
source('Simulations_Dataset.R')
source('Simulations_Estimation.R')
source('Kelp CPT - Peniche.R')
source('Kelp CPT - Sines.R')
source('Kelp CPT - Viana do Castelo.R')
source('Seagrass CPT.R')
 