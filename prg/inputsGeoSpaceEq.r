rm(list=ls())

setwd("~/Documents/GitHub/STModel-Analyzes/")

### Init libraries
require(sp)
require(raster)

#### Get and prep Input files
##############################################################################################

###### Clim
######################

geoGrid <- read.csv("./data/SDMClimate_grid.csv")
geoGrid <- geoGrid[,c("lat","lon","annual_mean_temp","tot_annual_pp")]
geoGrid <- subset(geoGrid,annual_mean_temp>=-4&annual_mean_temp<=10)
geoGrid <- subset(geoGrid,tot_annual_pp>=720&tot_annual_pp<=1200)

# Proj geoGrid 
coordinates(geoGrid) <- ~lon+lat
proj4string(geoGrid) <- CRS("+init=epsg:4326 +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")

# Set as raster
geoGrid_tp <- geoGrid[1]
geoGrid_pp <- geoGrid[2]
gridded(geoGrid_tp) <- gridded(geoGrid_pp) <- TRUE 
geoGrid <- stack(raster(geoGrid_tp),raster(geoGrid_pp))
NAvalue(geoGrid) <- -9999

### Crop on area
ext <- extent(c(-80,-60,43,51))
geoGrid <- crop(geoGrid,ext)

### Set to STM grid 
geoClimGrid <- as.data.frame(geoGrid,xy=TRUE)
geoClimGrid$lon <- geoClimGrid$x
geoClimGrid$lat <- geoClimGrid$y
geoClimGrid$x <- as.numeric(as.factor(geoClimGrid$x))
geoClimGrid$y <- as.numeric(as.factor(geoClimGrid$y))
geoClimGrid$x <- geoClimGrid$x -1
geoClimGrid$y <- geoClimGrid$y -1

geoCoordsGrid <- geoClimGrid[,c("x","y","lon","lat")]

### set NA cell with no variable climatic
geoClimGrid[is.na(geoClimGrid$annual_mean_temp),"tot_annual_pp"] <- NA
geoClimGrid[is.na(geoClimGrid$tot_annual_pp),"annual_mean_temp"] <- NA

### Scale geoClimGrid
load("./data/scale_info.Robj")
geoClimGrid$annual_mean_temp <- (geoClimGrid$annual_mean_temp-vars.means['annual_mean_temp'])/vars.sd['annual_mean_temp']
geoClimGrid$tot_annual_pp <- (geoClimGrid$tot_annual_pp-vars.means['tot_annual_pp'])/vars.sd['tot_annual_pp']

### set NA as -9999
geoClimGrid[is.na(geoClimGrid$annual_mean_temp),c("annual_mean_temp","tot_annual_pp")] <- -9999

### Write geoGrid
names(geoClimGrid)[3:4] <- c("env1","env2")
geoClimGrid$year <- 0
geoClimGrid <- geoClimGrid[,c("x","y","year","env1","env2")]
write.csv(geoClimGrid,"./data/init_geoClimGrid.csv",row.names=FALSE)

##### Landscape
######################

init_land_x <- seq(0,(max(geoClimGrid$x)+1)*10-1,1)
init_land_y <- seq(0,(max(geoClimGrid$y)+1)*10-1,1)
init_land_grid <- expand.grid(init_land_x,init_land_y)
init_grid <- data.frame(x=init_land_grid[,1],y=init_land_grid[,2],state=as.character("M"),stringsAsFactors=FALSE)

write.table(init_grid,"./data/init_geoGrid.csv",sep=",",row.names=FALSE,col.names=FALSE,quote=FALSE)

save(init_grid,geoClimGrid,geoCoordsGrid,file="./data/inputGeoSimu.rdata")