## Development of landscape functions

# Load packages
require(ggplot2)
require(RColorBrewer)
require(raster)
require(rgdal)
require(SDMTools)
require(reshape2)
require(plyr)
require(maptools)

# Load scripts and data
source("fcts_grid_visu.r")
source("fcts_land_analyze.r")
load("./data/class_sdm_proj.rdata")
load("./data/shapeFiles_forcrop.rdata")

proj <- subset(pred,mod=='RF')
names(proj)[3] <- 'state'
split.df <- split(proj,as.factor(proj$lon))
probs = c(0,.1,.2,.8,.9,1)

boundaries <- rbind.fill(lapply(split.df,bounds,probs=probs))
boundaries$group <- paste0(boundaries$perc,boundaries$state)
boundaries$group <- as.factor(boundaries$group)



cols <- brewer.pal(nlevels(proj$state),"Spectral")

state_M <- ggplot(proj,aes(x=lon,y=lat,fill=state)) +
        geom_polygon(data = df.countries, aes(x = long, y = lat, group = group),
            fill="grey80",colour="grey50",size=0.1) +
        geom_raster(alpha=0.2) +
        geom_line(data=subset(boundaries,state=='M'),aes(x=lon,y=lat,group=group,color=group),size=1)+
        scale_fill_manual(values=cols,name='State') +
        geom_polygon(data = subset(df.lakes,hole==FALSE),
            aes(x = long, y = lat, group = group),fill="light blue",
            colour="dodgerblue4",size=0.1) +
        scale_x_continuous(expand=c(0,0))+
        scale_y_continuous(expand=c(0,0))+
        coord_equal() +
        xlab("Longitude") + ylab("Latitude")+
        theme_df

state_T <- ggplot(proj,aes(x=lon,y=lat,fill=state)) +
        geom_polygon(data = df.countries, aes(x = long, y = lat, group = group),
            fill="grey80",colour="grey50",size=0.1) +
        geom_raster(alpha=0.2) +
        geom_line(data=subset(boundaries,state=='T'),aes(x=lon,y=lat,group=groupcd DocumenST,color=group),size=1)+
        scale_fill_manual(values=cols,name='State') +
        geom_polygon(data = subset(df.lakes,hole==FALSE),
            aes(x = long, y = lat, group = group),fill="light blue",
            colour="dodgerblue4",size=0.1) +
        scale_x_continuous(expand=c(0,0))+
        scale_y_continuous(expand=c(0,0))+
        coord_equal() +
        xlab("Longitude") + ylab("Latitude")+
        theme_df

state_B <- ggplot(proj,aes(x=lon,y=lat,fill=state)) +
        geom_polygon(data = df.countries, aes(x = long, y = lat, group = group),
            fill="grey80",colour="grey50",size=0.1) +
        geom_raster(alpha=0.2) +
        geom_line(data=subset(boundaries,state=='B'),aes(x=lon,y=lat,group=group,color=group),size=1)+
        scale_fill_manual(values=cols,name='State') +
        geom_polygon(data = subset(df.lakes,hole==FALSE),
            aes(x = long, y = lat, group = group),fill="light blue",
            colour="dodgerblue4",size=0.1) +
        scale_x_continuous(expand=c(0,0))+
        scale_y_continuous(expand=c(0,0))+
        coord_equal() +
        xlab("Longitude") + ylab("Latitude")+
        theme_df

#######################
# Use Contour Lines on prob

load("./data/prob_sdm_proj.rdata")

rs_probs <- subset(pred, mod =='RF')[,-7]
coordinates(rs_probs) <- ~ lon + lat
gridded(rs_probs) <- TRUE
rs_probs <- stack(rs_probs)

lev <- rasterToContour(rs_probs$T)@data

plines_T <- fortify(rasterToContour(rs_probs$T,maxpixels=50))
plines_M <- fortify(rasterToContour(rs_probs$M,maxpixels=50))
plines_R <- fortify(rasterToContour(rs_probs$R,maxpixels=50))
plines_B <- fortify(rasterToContour(rs_probs$B,maxpixels=50))

plines_R <- data.frame(plines_R,state=rep('R',nrow(plines_R)))
plines_M <- data.frame(plines_M,state=rep('M',nrow(plines_M)))
plines_T <- data.frame(plines_T,state=rep('T',nrow(plines_T)))
plines_B <- data.frame(plines_B,state=rep('B',nrow(plines_B)))

plines <-rbind(plines_R,plines_T,plines_M,plines_B)

ggplot(subset(plines,id=='C_9' | id=='C_10' | id=='C_8'),aes(x=long,y=lat)) + facet_wrap(~state) +
        geom_polygon(data = df.countries, aes(x = long, y = lat, group = group),
            fill="grey80",colour="grey50",size=0.1) +
        geom_line(aes(x=long,y=lat,group=group,color=id),size=1)+
        geom_polygon(data = subset(df.lakes,hole==FALSE),
            aes(x = long, y = lat, group = group),fill="light blue",
            colour="dodgerblue4",size=0.1) +
        scale_x_continuous(expand=c(0,0))+
        scale_y_continuous(expand=c(0,0))+
        coord_equal() +
        xlab("Longitude") + ylab("Latitude")+
        theme_df