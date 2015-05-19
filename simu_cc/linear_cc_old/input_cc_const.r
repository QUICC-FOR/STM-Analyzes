## Simulation CC with linear changements
rm(list=ls())
setwd( "/home/steve/Documents/GitHub/STModel-Analyzes/")

clim_cc <- read.csv("./data/init_geoClimGrid.csv")
clim_cc[which(clim_cc$env1=="-9999"),"env1"] <- NA
clim_cc[which(clim_cc$env2=="-9999"),"env2"] <- NA

load("./data/scale_info.Robj")

clim_cc$env1 <- clim_cc$env1 * vars.sd['annual_mean_temp'] + vars.means['annual_mean_temp']
clim_cc$year <- clim_cc$year + 1
inc <- 4/20
ls_out <- list()

for (i in 1:20){
    clim_out = clim_cc
    clim_out$env1 <- clim_out$env1 + i * inc
    clim_out$year <- i
    # Rescale variable
    clim_out$env1 <- (clim_out$env1 - vars.means['annual_mean_temp'])/vars.sd['annual_mean_temp']
    ls_out[[i]] <- clim_out
}

futclim <- do.call(rbind,ls_out)
futclim$year <- futclim$year-1
futclim[is.na(futclim$env1),c("env1","env2")] <- -9999

write.csv(futclim,"./simu_cc/linear_cc_old/init_fut_clim.csv",row.names=FALSE)


#### Prep landscape at the equilibrium

load("./outputs/GEO_GenSA_rf_0338_3_5y_tp_500.rdata")

step = nlayers(st_rs)
grid <- as.data.frame(st_rs[[step]],xy=TRUE)
names(grid) <- c("lon","lat","state")

# Reverse transfo
grid[which(grid$state==1),"state"] <- "B"
grid[which(grid$state==2),"state"] <- "T"
grid[which(grid$state==3),"state"] <- "M"
grid[which(grid$state==4),"state"] <- "R"
grid[is.na(grid$state),"state"] <- 0

grid$x <- as.numeric(as.factor(grid$lon))-1
grid$y <- as.numeric(as.factor(grid$lat))-1

init_grid <- grid[,c("x","y","state")]

write.table(init_grid,"./simu_cc/linear_cc_old/init_fut_land.csv",quote=FALSE,sep=",",row.names=FALSE,col.names=FALSE)