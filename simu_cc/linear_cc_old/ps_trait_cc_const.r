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

grid_cc <- read.csv("./temp_files/grid_final_cc.txt",header=FALSE,stringsAsFactors=FALSE)

names(grid_cc) <- c("x","y","state")

grid_cc[which(grid_cc$state=="B"),"state"] <- "1"
grid_cc[which(grid_cc$state=="T"),"state"] <- "2"
grid_cc[which(grid_cc$state=="M"),"state"] <- "3"
grid_cc[which(grid_cc$state=="R"),"state"] <- "4"
grid_cc[which(grid_cc$state=="0"),"state"] <- NA
grid_cc$state <- as.numeric(grid_cc$state)  

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

### Create matrix
grid <- acast(grid_cc,rev(y)~x,value.var = "state")
grid <- raster(grid)
proj4string(grid) <- CRS("+init=epsg:4269 +proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs +towgs84=0,0,0")
extent(grid) <- c(-79.95454,-60.04625,43.04572,50.95411)
grid <- mask(grid,countries)
grid <- as.data.frame(grid,xy=TRUE,na.rm=TRUE)
names(grid) <- c("lon","lat","state")

grid[which(grid$state==1),"state"] <- "B"
grid[which(grid$state==2),"state"] <- "T"
grid[which(grid$state==3),"state"] <- "M"
grid[which(grid$state==4),"state"] <- "R"

grid$state <- as.factor(grid$state)


theme_set(theme_grey(22))

dist_state <- visu_state_map(grid,"State") 
ggsave(dist_state,file="./figures/338_withcc_final_land.pdf",width=8,height=3.5)