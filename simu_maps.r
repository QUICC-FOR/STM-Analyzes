rm(list=ls())

require(stringr)
require(raster)
require(reshape2)

setwd("/home/steve/Documents/GitHub/STModel-Analyzes")

simu_path <- "./STModel-Simulation/out_files/"
list_dirs <-as.character(dir(simu_path,recursive=TRUE))

simu <- str_extract(do.call(rbind,str_split(list_dirs,"/"))[,2],"(\\d)+")
step <- str_extract(do.call(rbind,str_split(list_dirs,"/"))[,3],"(\\d)+")
list_dirs <- data.frame(path=list_dirs,simu=simu,step=step,stringsAsFactors=FALSE)

ext_simu <- extent(-96.998826,-57.308128,41.000000,52.900101)

# Get simu data
dat <- read.csv(paste(simu_path,list_dirs[1,1],sep=""),header=FALSE,stringsAsFactors=FALSE)
names(dat) <- c("x","y","state")

dat$state[dat$state=="B"] <- "1"
dat$state[dat$state=="T"] <- "2"
dat$state[dat$state=="M"] <- "3"
dat$state[dat$state=="R"] <- "4"
dat$state[dat$state=="0"] <- NA
dat$state <- as.numeric(dat$state)  

dat <- acast(dat,y~x,value.var = "state")
rs_dat <- raster(dat)
extent(rs_dat) <- ext_simu
projection(rs_dat) <- CRS("+init=epsg:4269")
df_dat <- as.data.frame(rs_dat,xy=TRUE, na.rm=TRUE)
names(df_dat) <- c("lon","lat","state")
df_dat$state <- as.factor(df_dat$state)

load("./data/shapeFiles_forcrop.rdata")

#Load functions
source("fcts_grid_visu.r")

visu_state_map(df_dat,"Spectral","Simulation (MN 0.33)","State")

