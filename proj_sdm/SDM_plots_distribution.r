## Load librairy
require("raster")
require("sp")
require("ggplot2")
require("rgdal")
require("RColorBrewer")
require("reshape2")

## load data
load("./STModel-Data/out_files/transitions_r1.rdata")
source("./fcts_grid_visu.r")

## Filtered unsued plots and columns
plots <-subset(stateData,stateData$annual_mean_temp<=10 & stateData$state != "U")
plots_coords <- plots[,c(4,3)]

# Get spatial points
spdf <- SpatialPoints(coords=plots_coords,
    proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs"))
spdf <- spTransform(spdf,CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs"))

# Create raster, count and return obj to df
rast <- raster(nrow=75,ncol=75,extent(spdf))
rast <- rasterize(spdf,rast,fun='count')
df.count <- as.data.frame(rast,xy=TRUE)

# Prepare shapefiles
# Crop lakes and countries on the area

lakes <- readOGR(dsn="./STModel-Data/out_files/shapefiles/",layer="lakes_stm_area")
countries <- readOGR(dsn="./STModel-Data/out_files/shapefiles/",layer="countries_stm_area")

ext <- extent(c(range(plots$lon),range(plots$lat)))

lakes <- crop(lakes,ext)
countries <- crop(countries,ext)

lakes <- spTransform(lakes,CRS('+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs'))
countries <- spTransform(countries,CRS('+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs'))

df.countries <- fortify(countries)
df.lakes <- fortify(lakes)

# Distribution map all year

df.count$class <- cut(df.count$layer,c(1,10, 50, 100, 200, 300 ,400, 500, 800))
#df.count$class <- factor(df.count$class,levels=rev(levels(df.count$class)))

cols <- brewer.pal(9,"YlOrRd")

plots_distrib_allyr <- ggplot(df.count,aes(x=x,y=y,fill=class)) +
        geom_polygon(data = df.countries, aes(x = long, y = lat, group = group),fill="grey80",colour="grey50",size=0.1) +
        geom_raster(alpha=0.7) +
        scale_fill_manual(values=cols,name="Count") +
        geom_polygon(data = subset(df.lakes,hole==FALSE),
            aes(x = long, y = lat, group = group),fill="light blue",
            colour="dodgerblue4",size=0.1) +
        scale_x_continuous(expand=c(0,0))+
        scale_y_continuous(expand=c(0,0))+
        coord_equal() +
        xlab("Longitude") + ylab("Latitude")+
        ggtitle("SDM Calibration \n Plots distribution for all years")+
        theme_df

ggsave(plots_distrib_allyr,file="./figures/distrib_plots_allyrs.jpg",width=8,height=6)

## Distribution plots by decades

plots_years <- plots[,c(4,3,2)]

# Cut to retrieve decades
plots_years$dec <- cut(plots_years$year_measured,c(1960,1970,1980,1990,2010,2012))
plots_years <- plots_years[,c(1,2,4)]

# Get spatial points

decades_count <- stack()

for (i in 1:nlevels(plots_years$dec)){
    dec_level <- levels(plots_years$dec)[i]
    plots_dec <- subset(plots_years,dec == dec_level)[,1:2]
    spdf <- SpatialPoints(coords=plots_dec,proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs"))
    rast <- raster(nrow=75,ncol=75,ext)
    rast <- rasterize(spdf,rast,fun='count')
    decades_count <- addLayer(decades_count,rast)
    names(decades_count)[i] <- dec_level
}


# Create raster, count and return obj to df

df.decades <- as.data.frame(decades_count,xy=TRUE)
df.decades <- melt(df.decades,id=c("x","y"))
df.decades$variable <- factor(df.decades$variable,labels=levels(plots_years$dec))
names(df.decades)[3:4] <- c("dec","count")
df.decades$count <- cut(df.decades$count ,c(1,10, 50, 100, 200, 300 ,400, 500, 800))


plots_distrib_by_decades <- ggplot(df.decades,aes(x=x,y=y,fill=count)) +
        facet_wrap(~dec)+
        geom_polygon(data = df.countries, aes(x = long, y = lat, group = group),fill="grey80",colour="grey50",size=0.1) +
        geom_raster(alpha=0.7) +
        scale_fill_manual(values=cols,name="Count") +
        geom_polygon(data = subset(df.lakes,hole==FALSE),
            aes(x = long, y = lat, group = group),fill="light blue",
            colour="dodgerblue4",size=0.1) +
        scale_x_continuous(expand=c(0,0))+
        scale_y_continuous(expand=c(0,0))+
        coord_equal() +
        xlab("Longitude") + ylab("Latitude") +
        ggtitle("SDM Calibration \n Plots distribution by decades")+
        theme_df

ggsave(plots_distrib_by_decades,file="./figures/distrib_plots_by_decades.jpg",width=12,height=6)

##### Bin maps by state

plots <-subset(stateData,stateData$annual_mean_temp<=10 & stateData$state != "U")
plots_coords_state <- plots[,c(4,3,9)]

# Get spatial points
spdf <- SpatialPointsDataFrame(coords=plots_coords_state[,1:2],data=plots_coords_state,proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs"))
spdf <- spTransform(spdf,CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs"))

# Create raster, count and return obj to df
rast <- raster(nrow=75,ncol=75,extent(spdf))
rast_B <- rasterize(as(subset(spdf,state=='B'),"SpatialPoints"),rast,fun='count')
rast_M <- rasterize(as(subset(spdf,state=='M'),"SpatialPoints"),rast,fun='count')
rast_T <- rasterize(as(subset(spdf,state=='T'),"SpatialPoints"),rast,fun='count')
st <- stack(rast_T,rast_B,rast_M)
names(st) <- c("T","B","M")

dfstate <- as.data.frame(st,xy=TRUE)
dfstate<-melt(dfstate,id=c("x","y"),value.name="count",variable.name="state")
dfstate[dfstate$count < 10 & !is.na(dfstate$count), "count" ] <- NA

dfstate$count <- cut(dfstate$count ,c(10, 50, 100, 200, 300 ,400, 500, 800))
dfstate$state <- factor(dfstate$state,labels=c("Temperate","Boreal","Mixed"))

cols <- brewer.pal(6,"YlOrBr")
distrib_state <- ggplot(dfstate,aes(x=x,y=y,fill=count)) + facet_wrap(~state,ncol=1) +
        geom_polygon(data = df.countries, aes(x = long, y = lat, group = group),fill="grey80",colour="grey50",size=0.1) +
        geom_raster(alpha=0.7) +
        scale_fill_manual(values=cols,name="Number of plots") +
        geom_polygon(data = subset(df.lakes,hole==FALSE),
            aes(x = long, y = lat, group = group),fill="light blue",
            colour="dodgerblue4",size=0.1) +
        scale_x_continuous(expand=c(0,0))+
        scale_y_continuous(expand=c(0,0))+
        coord_equal() +
        xlab("Longitude") + ylab("Latitude")+
        theme_df


tiff("./figures/distrib_state.tiff", width = 5, height = 8, units = 'in', res = 300)
print(distrib_state)
dev.off()