### Post traitement constante
source("./prg/wrap_fcts.r")
source("./prg/fcts_grid_visu.r")

require(raster)
require(rgdal)
require(ggmap)
require(ggplot2)
require(reshape2)
require(rgeos)
require(sp)

# land <- read.csv("./simu_cc/linear_cc_old/init_fut_land.csv",header=FALSE)
# names(land) <- c("x","y","state")
# clim_cc <- read.csv("./simu_cc/linear_cc_old/init_fut_clim.csv")

# x <- max(land$x)+1
# y <- max(land$y)+1
# a <- max(clim_cc$x)+1
# b <- max(clim_cc$y)+1
# pars<-"GenSA_rf_0.338_3_5y.txt"
# clim <- "./simu_cc/linear_cc_old/init_fut_clim.csv"
# land_file <-"./simu_cc/linear_cc_old/init_fut_land.csv"
# writeStep <- 20
# disturb <- 0
# transProb <- 1

# stmodel(params=pars,inland=land_file,outland="./simu_cc/fut_land_20_ts.csv",disturb=disturb,x=x,y=y,a=a,b=b,clim_file=clim,writeStep=writeStep,transProb=transProb,const_clim=FALSE)

### Create fut land
grid_cc <- read.csv("./simu_cc/fut_land_20_ts.txt",header=FALSE,stringsAsFactors=FALSE)
names(grid_cc) <- c("x","y","state")
grid_cc$state <- stateToId(grid_cc$state)

### Create 

# Prep shp cover
#Crop lakes and countries on the area
lakes <- readOGR(dsn="./data/shapefiles/",layer="lakes_stm_area")
countries <- readOGR(dsn="./data/shapefiles/",layer="countries_stm_area")

ext <- extent(c(-79.95454,-60.04625,43.04572,50.95411))
lakes <- crop(lakes,ext)
countries <- crop(countries,ext)
countries <- gSimplify(countries,tol=0.01)
df.lakes <- fortify(lakes)
df.countries <- fortify(countries)


### Create present land
#grid <- read.csv("./simu_cc/linear_cc_old/init_fut_land.csv",header=FALSE)
grid <- read.csv("./simu_cc/fut_land_20_ts.csv",header=FALSE)
names(grid) <- c("x","y","state")
grid$state <- stateToId(grid$state) 

grid <- acast(grid,rev(y)~x,value.var = "state")
grid <- raster(grid)
proj4string(grid) <- CRS("+init=epsg:4269 +proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs +towgs84=0,0,0")
extent(grid) <- c(-79.95454,-60.04625,43.04572,50.95411)
grid <- mask(grid,countries)

# create contour for present land
class<-data.frame(min=c(1,2,3,4),max=c(1,2,3,4),nclass=c(0,1,1,0))


# Prep for ggplot
grid <- as.data.frame(grid,xy=TRUE,na.rm=TRUE)
names(grid) <- c("lon","lat","state")

grid$state <- idToState(grid$state)
grid$state <- as.factor(grid$state)



dist_state <- visu_state_map(grid,"State") 
ggsave(dist_state,file="./figures/338_withcc_final_land.pdf",width=8,height=3.5)
