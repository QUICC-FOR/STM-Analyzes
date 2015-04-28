rm(list=ls())

setwd("~/Documents/GitHub/STModel-Analyzes/")
source("./prg/wrap_fcts.r")

### Init libraries
require(sp)
require(raster)
require(reshape2)


#### Init constants
##############################################################################################

ls_pars <- c(
"GenSA_rf_0.338_3_5y.txt"
)

inland= "./data/init_geoGrid.csv"
clim_file= "./data/init_geoClimGrid.csv"
writeStep=10
disturb=0
transProb=1
timeSteps=500


#### Run simu/outputs/figs
##############################################################################################

for (i in 1:length(ls_pars)){
    cat("Running:",ls_pars[i],"\n")
    runGeoSimu(params=ls_pars[i],timeSteps=timeSteps,transProb=transProb,writeStep=writeStep,disturb=disturb,clim_file=clim_file,land_file=inland)
}