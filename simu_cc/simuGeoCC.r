### Run STM on scenario: rcp60

### set wd
setwd("~/Documents/GitHub/STModel-Analyzes/")
rm(list=ls())
source("./prg/wrap_fcts.r")

### Get list timesteps files 
files <- list.files("./simu_cc/input_clim/")
paths <- list.files("./simu_cc/input_clim/", full.names = TRUE)
df_ts <- data.frame(file=files,path=paths,id=do.call(rbind,strsplit(files,"[._]"))[,1], yr=do.call(rbind,strsplit(files,"[._]"))[,3],stringsAsFactors=FALSE)
df_ts$id <- as.factor(df_ts$id)

### Set STM pars
land_file <- "./simu_cc/init_land_cc.csv" 

### Load inputs
land <- read.csv(land_file,header=FALSE,sep=" ")
names(land) <- c("x","y","state")
clim <- read.csv(df_ts[1,"path"])

### Set others STM pars
x <- max(land$x)+1
y <- max(land$y)+1
a <- max(clim$x)+1
b <- max(clim$y)+1
pars<-"GenSA_rf_0.338_3_5y.txt"
writeStep <- 10
disturb <- 0
transProb <- 1

#for (i in 1:nlevels(df_ts$id)){
    i=1 
    cc_files <- subset(df_ts,id==levels(df_ts$id)[i])
    cc_files <- cc_files[order(cc_files$yr),]

    ## Run first step
    stmodel(params=pars,inland="./data/init_geoCCGrid.csv",outland=paste("./temp_files/",levels(df_ts$id)[i],"_id_1_step.txt",sep=""),disturb=disturb,x=x,y=y,a=a,b=b,clim_file=,writeStep=writeStep,transProb=transProb)

    ## Run CC


#}