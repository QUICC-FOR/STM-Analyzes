## Prepare cc
setwd("~/Documents/GitHub/STModel-Analyzes/")
rm(list=ls())

require(raster)
require(rgdal)
require(sp)
require(data.table)

source("./prg/wrap_fcts.r")

list_cc_files <- data.frame(path=list.files("./data/fut_clim_rcp60/",full.names=TRUE),name=list.files("./data/fut_clim_rcp60/"), stringsAsFactors=FALSE)
list_cc_files$yr <- do.call(rbind,strsplit(list_cc_files$name,"[-_.]"))[,7]
list_cc_files$id <- do.call(rbind,strsplit(list_cc_files$name,"[-_.]"))[,4]

# Load scale
load('./data/scale_info.Robj')

# Manage Land 
load('./outputs/GEO_GenSA_rf_0338_3_5y_tp_500.rdata')
land <- st_rs[[nlayers(st_rs)]]
common_ext <- extent(c(-79.95869,-59.61305,43.04159,50.95823))
land <- crop(land,common_ext)

# Prep land for input
# land <- as.data.frame(land,xy=TRUE,na.rm=FALSE)
# land$x <- as.numeric(as.factor(land$x))-1
# land$y <- as.numeric(as.factor(land$y))-1
# names(land)[3] <- "state"
# land$state <- idToState(land$state)
# land[is.na(land$state),"state"] <- 0 

# write.table(land,file="./simu_cc/init_land_cc.txt",row.names=FALSE,col.names=FALSE,quote=FALSE)

for (i in 1:dim(list_cc_files)[1]){
    cc_file <- list_cc_files[i,"path"]
    cc_dat <- as.data.frame(fread(cc_file))

    annual_tp <- subset(cc_dat,var=="bio1")
    annual_pp <- subset(cc_dat,var=="bio12")

    rs_tp <- annual_tp[,c("lon","lat","val")]
    rs_pp <- annual_pp[,c("lon","lat","val")]
    rs_tp[which(rs_tp$val==-9999),"val"] <- NA
    rs_pp[which(rs_pp$val==-9999),"val"] <- NA

    coordinates(rs_tp) <- coordinates(rs_pp) <- ~lon+lat
    gridded(rs_tp) <- gridded(rs_pp) <- TRUE
    rs_tp <- raster(rs_tp)
    rs_pp <- raster(rs_pp)
    projection(rs_tp) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs;")
    projection(rs_pp) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs;")

    # Resample clim on land
    ###########################################

    rs_tp <- projectRaster(rs_tp,crs=CRS("+init=epsg:4269 +proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs +towgs84=0,0,0"))
    rs_pp <- projectRaster(rs_pp,crs=CRS("+init=epsg:4269 +proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs +towgs84=0,0,0"))
    
    clim_align <- raster(nrow=nrow(land)/10,ncol=ncol(land)/10,ext=common_ext)

    clim <- stack(rs_tp,rs_pp)
    clim <- resample(clim,clim_align)

    ## Transform to output 
    ###########################################

    out <- as.data.frame(clim,xy=TRUE,na.rm=FALSE)
    names(out) <- c("x","y","env1","env2")
    out[is.na(out$env1),"env2"] <- NA
    out[is.na(out$env2),"env1"] <- NA
    out$year <- rep(0,dim(out)[1])

    # restruct dataframe
    ###########################################
    out <- out[,c("x","y","year","env1","env2")]
    out$x <- as.numeric(as.factor(out$x))-1
    out$y <- as.numeric(as.factor(out$y))-1

    # scale
    ###########################################
    out$env1 <- (out$env1 - vars.means['annual_mean_temp']) / vars.sd['annual_mean_temp']
    out$env2 <- (out$env2 - vars.means['tot_annual_pp']) / vars.sd['tot_annual_pp']

    # set NA
    ###########################################
    out[is.na(out$env1),c("env1","env2")] <- -9999
    
    # export
    ###########################################
    write.csv(out,file=paste("./simu_cc/input_clim/", list_cc_files[i,"id"],"_yr_",list_cc_files[i,"yr"],".csv",sep=""),row.names=FALSE)

}