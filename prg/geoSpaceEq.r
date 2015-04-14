rm(list=ls())

setwd("~/Documents/GitHub/STModel-Analyzes/")
source("./prg/wrap_fcts.r")

### Init libraries
require(sp)
require(raster)
require(reshape2)

### Load Input
#load("./data/inputGeoSimu.rdata")


#### Init constants
##############################################################################################

ls_pars <- c("GenSA_initForFit_rf_0.3315y2.txt","GenSA_initForFit_rf_0.3322.txt","GenSA_initForFit_rf_0.3325y.txt","GenSA_initForFit_rf_0.333.txt","GenSA_initForFit_rf_0.3332.txt","GenSA_initForFit_rf_0.3335y.txt","GenSA_initForFit_rf_0.3335y2.txt","GenSA_initForFit_rf_0.3345y.txt","GenSA_initForFit_rf_0.335.txt","GenSA_initForFit_rf_0.3352.txt","GenSA_initForFit_rf_0.3355y.txt","GenSA_initForFit_rf_0.3365y.txt","GenSA_initForFit_rf_0.3375y.txt","GenSA_initForFit_rf_0.3385y.txt","GenSA_initForFit_rf_0.339.txt","GenSA_initForFit_rf_0.3392.txt","GenSA_initForFit_rf_0.3395y.txt")


inland= "./data/init_geoGrid.csv"
clim_file= "./data/init_geoClimGrid.csv"
writeStep=10
disturb=0
transProb=1
timeSteps=1000


#### Run simu/outputs/figs
##############################################################################################

for (i in 1:length(ls_pars)){
    cat("Running:",ls_pars[i],"\n")
    runGeoSimu(params=ls_pars[i],timeSteps=timeSteps,transProb=transProb,writeStep=writeStep,disturb=disturb,clim_file=clim_file,land_file=land_file)
}