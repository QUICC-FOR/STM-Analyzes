## Visu equilibrium maps

setwd("~/Documents/GitHub/STModel-Analyzes/")
source("./prg/wrap_fcts.r")
source("./prg/fcts_grid_visu.r")

require(raster)
require(rgdal)
require(ggmap)
require(ggplot2)
require(reshape2)

# Prep shp cover
#Crop lakes and countries on the area
lakes <- readOGR(dsn="./data/shapefiles/",layer="lakes_stm_area")
countries <- readOGR(dsn="./data/shapefiles/",layer="countries_stm_area")
# dem <- readGDAL("./data/dem_usgc_stm.tif")
# dem <- as.data.frame(raster(dem),xy=TRUE,na.rm=TRUE)
# names(dem) <- c("lon","lat","elev")

ext <- extent(c(-79.95454,-60.04625,43.04572,50.95411))

lakes <- crop(lakes,ext)
countries <- crop(countries,ext)
countries <- gSimplify(countries,tol=0.01)
df.lakes <- fortify(lakes)
df.countries <- fortify(countries)


# save(dem,df.countries,df.lakes,file="./data/shp_stm_area.rdata")

theme_set(theme_grey(22))

ls_out <- c(
"GEO_GenSA_rf_0338_3_5y_tp_500.rdata")


for (i in 1:length(ls_out)){
    i=1
    out <- ls_out[i]
    load(paste("./outputs/",out,sep=""))
    #load("./data/shp_stm_area.rdata")

    step = nlayers(st_rs)
    rs <- mask(st_rs[[step]],countries)
    df_out <- as.data.frame(rs,xy=TRUE,na.rm=TRUE)
    names(df_out) <- c("lon","lat","state")

    # Reverse transfo
    df_out[which(df_out$state==1),"state"] <- "B"
    df_out[which(df_out$state==2),"state"] <- "T"
    df_out[which(df_out$state==3),"state"] <- "M"
    df_out[which(df_out$state==4),"state"] <- "R"

    df_out$state <- as.factor(df_out$state)

    dist_state <- visu_state_map(df_out,"State") 
    ggsave(dist_state,file="./figures/338_500ts_final_land.pdf",width=8,height=3.5)


    freq_time <- freq(st_rs,merge=TRUE)

    freq_time[which(freq_time$value==1),"value"] <- "B"
    freq_time[which(freq_time$value==2),"value"] <- "T"
    freq_time[which(freq_time$value==3),"value"] <- "M"
    freq_time[which(freq_time$value==4),"value"] <- "R"


    freq_time <- melt(freq_time,id="value",variable.name="step",value.name="count")
    names(freq_time)[1] <- "state"
    freq_time$count <- freq_time$count/ncell(rs)
    freq_time$step <- as.numeric(gsub("[^0-9]", "", as.character(freq_time$step)))
    freq_time$state <- as.factor(freq_time$state)

    prop_state <- ggplot(freq_time,aes(x=step,y=count,group=state,color=state))+ geom_line(size=0.5) +
    scale_color_manual(values=pal_state,name="State") +
     xlab("Timestep") + 
     ylab("States proportion") +  
     theme_few()

    g <- arrangeGrob(dist_state,prop_state)
    
    dev.off()
    
    ggsave(paste("figures/",strsplit(out, "\\.")[[1]][1],".jpg",sep=""),g,dpi=300,width=8)

    dev.off()

    
}