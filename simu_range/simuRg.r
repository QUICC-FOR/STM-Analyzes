## Prep 
setwd("~/Documents/GitHub/STModel-Analyzes/")
rm(list=ls())
system("rm temp_files/*")

### set workspace 
source("./prg/wrap_fcts.r")
source("./prg/fcts_grid_visu.r")

### Required libraries 
require(raster)
require(rgdal)
require(ggplot2)
require(stringr)
require(reshape2)

### Init study area 
study_area <- readOGR(dsn="./simu_range/","ext_area")
ext_study_area <- extent(study_area)

###### Land
######################

### Init land
load("./outputs/GEO_GenSA_rf_0338_3_5y_tp_500.rdata")
init_land <- st_rs[[nlayers(st_rs)]]
rm(st_rs)
init_land <- crop(init_land,ext_study_area)

### Plot initial landscape
land <- as.data.frame(init_land,xy=TRUE)
names(land) <- c("lon","lat","state")
land$state <- idToState(land$state)

landmap <- ggplot(land,aes(y=lon,x=lat,fill=state)) +
    geom_raster() + 
    scale_fill_manual(values=pal_state) +
    theme_df +
    scale_x_continuous(expand=c(0,0))+
    scale_y_continuous(expand=c(0,0))+
    coord_equal() +
    xlab("Longitude") + ylab("Latitude")

# Prep land as input
land$x <- as.numeric(as.factor(land$lon))-1
land$y <- as.numeric(as.factor(land$lat))-1 
land$state <- as.character(land$state)


# # Write input 
# write.table(land[,c("x","y","state")],file="./simu_range/init_70-00_landRgGrid.csv",sep=",",quote=FALSE,col.names=FALSE,row.names=FALSE)


### Init clim
###### Clim
######################

climRg <- read.csv("./data/SDMClimate_grid.csv")
climRg <- climRg[,c("lat","lon","annual_mean_temp","tot_annual_pp")]
climRg <- subset(climRg,annual_mean_temp>=-4&annual_mean_temp<=10)
climRg <- subset(climRg,tot_annual_pp>=720&tot_annual_pp<=1200)

# Proj climRg 
coordinates(climRg) <- ~lon+lat
proj4string(climRg) <- CRS("+init=epsg:4326 +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")

# Set as raster
climRg_tp <- climRg[1]
climRg_pp <- climRg[2]
gridded(climRg_tp) <- gridded(climRg_pp) <- TRUE 
climRg_pp <- raster(climRg_pp)
climRg_tp <- raster(climRg_tp)
NAvalue(climRg_pp) <- NAvalue(climRg_tp) <- -9999

### Resample on area
climRg_tp <- resample(climRg_tp,init_land)
climRg_pp <- resample(climRg_pp,init_land)

###### Comment to get real climate
####################################################################

# # Get proj and ext
# ext <- extent(climRg_pp)
# proj <- proj4string(climRg_pp)

# # Create tp raster
# rg_test_tp <- range(climRg_tp@data@values)
# grad_tp <- seq(rg_test_tp[1],rg_test_tp[2],length.out=dim(climRg_tp)[1])
# grid_grad_tp <- matrix(nrow =dim(climRg_tp)[1], ncol =dim(climRg_tp)[2])
# for (i in 1:dim(climRg_tp)[2]){
#     grid_grad_tp[,i] <- grad_tp
# }

# # Create pp raster
# rg_test_pp <- range(climRg_pp@data@values)
# grad_pp <- seq(rg_test_pp[1],rg_test_pp[2],length.out=dim(climRg_pp)[1])
# grid_grad_pp <- matrix(nrow =dim(climRg_pp)[1], ncol =dim(climRg_pp)[2])
# for (i in 1:dim(climRg_pp)[2]){
#     grid_grad_pp[,i] <- grad_pp
# }

# #tranfo to raster
# climRg_tp <- raster(grid_grad_tp)
# climRg_pp <- raster(grid_grad_pp)
# extent(climRg_tp) <- extent(climRg_pp) <- ext
# proj4string(climRg_tp) <- proj4string(climRg_pp) <- proj

####################################################################
climRg <- stack(climRg_tp,climRg_pp)

### Set to STM grid 
climRgGrid <- as.data.frame(climRg,xy=TRUE)
climRgGrid$x <- as.numeric(as.factor(climRgGrid$x)) -1
climRgGrid$y <- as.numeric(as.factor(climRgGrid$y)) -1
names(climRgGrid)[3:4] <- c("annual_mean_temp","tot_annual_pp")

### set NA cell with no variable climatic
climRgGrid[is.na(climRgGrid$annual_mean_temp),"tot_annual_pp"] <- NA
climRgGrid[is.na(climRgGrid$tot_annual_pp),"annual_mean_temp"] <- NA

### Increase temp
climRgGrid[,'annual_mean_temp'] <- climRgGrid[,'annual_mean_temp'] + 2

### Scale climRgGrid
load("./data/scale_info.Robj")
climRgGrid$annual_mean_temp <- (climRgGrid$annual_mean_temp-vars.means['annual_mean_temp'])/vars.sd['annual_mean_temp']
climRgGrid$tot_annual_pp <- (climRgGrid$tot_annual_pp-vars.means['tot_annual_pp'])/vars.sd['tot_annual_pp']

### set NA as -9999
climRgGrid[is.na(climRgGrid$annual_mean_temp),c("annual_mean_temp","tot_annual_pp")] <- -9999

### Write climRg
names(climRgGrid)[3:4] <- c("env1","env2")
climRgGrid$year <- 0
climRgGrid <- climRgGrid[,c("x","y","year","env1","env2")]
write.csv(climRgGrid,"./simu_range/init_realClim_wo_cc.csv",row.names=FALSE)


#####################################################
### Get land at eq with new clim

system("./prg/stmodel -x 27 -y 587 -a 27 -b 587 -s -c ./simu_range/init_fakeClim_wo_cc.csv -p ./pars/GenSA_rf_0.338_3_5y.txt -i ./simu_range/init_70-00_landRgGrid.csv -t 1000 -d 0 -e 1 > ./simu_range/init_eq_landRgGrid.csv")


### Run model

# Build string 

x <- max(land$x)+1
y <- max(land$y)+1
a <- max(climRgGrid$x)+1
b <- max(climRgGrid$y)+1
pars<-"GenSA_rf_0.338_3_5y.txt"
clim_file <- "./simu_range/init_fakeClim_wt_cc.csv"
land_file <-"./simu_range/init_eq_landRgGrid.csv"
writeStep <- c(10,20,80,100,200,500,1000)
nrep <- seq(1,10,1)
disturb <- 0
transProb <- 1

for (j in 1:length(nrep)){
    for (i in 1:length(writeStep)){
        stmodel(params=pars,inland=land_file,outland=paste("./temp_files/",nrep[j],"_rp_",writeStep[i],"_ts_eqland_with_cc_",pars,sep=""),disturb=disturb,x=x,y=y,a=a,b=b,clim_file=clim_file,writeStep=writeStep[i],transProb=transProb)
    }
}

# Manage outputs
path <- list.files("./temp_files/",full.names = TRUE)
files <- list.files("./temp_files/")
rp<- do.call(rbind,strsplit(files, "_"))[,1]
ts<- do.call(rbind,strsplit(files, "_"))[,3]

df_out <- data.frame(path=path,ts=as.integer(ts),rep=as.integer(rp),stringsAsFactors=FALSE)
df_out <- df_out[order(df_out$ts),]
ls_rs <- list()

for (i in 1:length(path)){
    predland <- read.csv(df_out[i,1],header=FALSE)
    names(predland) <- c("x","y","state")
    predland$state <- stateToId(as.character(predland$state))
    predland <- acast(predland,rev(y)~x,value.var = "state")
    predland <- raster(predland)
    proj4string(predland) <- CRS("+init=epsg:4326 +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")
    extent(predland) <- extent(climRg)
    names(predland) <- paste('Landscape after:',df_out[i,2],"- rep:",df_out[i,3])
    ls_rs[[i]] <- predland
}

st_rs <- stack(ls_rs)
save(st_rs,file=paste("./outputs/RG_eqLand_fakeClim_rep_",length(nrep),"_",strsplit(pars, "\\.")[[1]][1],"_ts_",min(writeStep),"_",max(writeStep),".rdata",sep=""))
system("rm temp_files/*")


### Figs
### Load stack and clim
#load('./outputs/RG_rep_10_GenSA_rf_0_ts_10_5000.rdata')
simu_grids <- as.data.frame(st_rs,xy=TRUE,na.rm=TRUE)
simu_grids <- melt(simu_grids,id=c("x","y"),value.name="state",variable.name="step")

vec_chr <- as.character(simu_grids$step)
vec_chr<- do.call(rbind,strsplit(vec_chr, "rep"))
vec_rep <- str_extract(vec_chr[,2],"[[:digit:]]+")
vec_ts <-  str_extract(vec_chr[,1],"[[:digit:]]+")

simu_grids$step <- vec_ts
simu_grids$rep <- vec_rep
simu_grids$step <- as.factor(simu_grids$step)
simu_grids$rep <- as.factor(simu_grids$rep)
simu_grids$state <- idToState(simu_grids$state)
simu_grids$state <- as.factor(simu_grids$state)

simumap <- ggplot(subset(simu_grids,rep==1),aes(y=x,x=y,fill=state)) +
    geom_raster() + 
    facet_wrap(~step,ncol=1)+
    scale_fill_manual(values=pal_state) +
    theme_df +
    scale_x_continuous(expand=c(0,0))+
    scale_y_continuous(expand=c(0,0))+
    coord_equal() +
    xlab("Longitude") + ylab("Latitude")