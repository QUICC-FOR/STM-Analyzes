rm(list=ls())

setwd("~/Documents/GitHub/STModel-Analyzes/")

#### Librairy
require(reshape2)
require(raster)
require(ggplot2)
require(gridExtra)
require(ggthemes)

#### Source functions
source("./prg/wrap_fcts.r")

#### Set List of params and constants

ls_pars <- c(
"GenSA_rf_0.331_3_1y.txt",
"GenSA_rf_0.332_3_1y.txt",
"GenSA_rf_0.333_3_1y.txt",
"GenSA_rf_0.334_3_1y.txt",
"GenSA_rf_0.335_3_1y.txt",
"GenSA_rf_0.336_3_1y.txt",
"GenSA_rf_0.337_3_1y.txt",
"GenSA_rf_0.338_3_1y.txt",
"GenSA_rf_0.339_3_1y.txt")


clim_file="./data/init_clim.csv"
land_file="./data/init_M_land.csv"
grain=100
timeSteps=12500

#### Generate Input files
##############################################################################################

# Set climatic space
rg_tp <- c(-4,10)
rg_pp <- c(750,1200)

#### Scale range of clim variable
load("./data/scale_info.Robj")
rg_tp <- (rg_tp - vars.means['annual_mean_temp'])/vars.sd['annual_mean_temp']
rg_pp <- (rg_pp - vars.means['tot_annual_pp'])/vars.sd['tot_annual_pp']

#### Create initial landscape grid 
coord_x <- coord_y <- seq(0,grain-1,1)
coord_grid <- expand.grid(coord_x,coord_y)

land <- data.frame(x=coord_grid[,1],y=coord_grid[,2],state='M')
write.table(land,land_file,sep=",",quote=FALSE,row.names=FALSE,col.names=FALSE)

#### Create initial clim landscape
env1 <- seq(rg_tp[1],rg_tp[2],length.out=grain)
env2 <- seq(rg_pp[1],rg_pp[2],length.out=grain)
clim_grid <- expand.grid(env1,env2)

clim <- data.frame(x=coord_grid[,1],y=coord_grid[,2],year=0,env1=clim_grid[,1],env2=clim_grid[,2])
write.table(clim,clim_file,sep=",",row.names=FALSE)


#### Run simu/outputs/figs
##############################################################################################

for (i in 1:length(ls_pars)){

    runClimSimu(params=ls_pars[i],grain=grain,timeSteps=timeSteps,transProb=1,writeStep=10,disturb=0,clim_file=clim_file,land_file=land_file)

    outname <- paste(strsplit(ls_pars[i], "\\.")[[1]][1],strsplit(ls_pars[i], "\\.")[[1]][2],"_tp_",timeSteps,"_gr_",grain,".rdata",sep="")

    getClimFigs(simu_out=outname)
}

