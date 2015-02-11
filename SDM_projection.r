# Projection of the SDM
# Date: 9th February, 2014

#SetWD

setwd("/home/steve/Documents/GitHub/STModel-Analyzes/")

###### Load librairies
require("ggplot2")
require("RColorBrewer")
require("randomForest")
require("nnet")
require("reshape2")
require("rgdal")
require("raster")

# load SDM and shapefiles
load('./STModel-Calibration/data/RandomForest_complete.rObj')
load('./STModel-Calibration/data/Multinom_complete.rObj')
load('./STModel-Calibration/scripts/scale_info.Robj')

# load data reshaped
load('./STModel-Data/out_files/transitions_r1.rdata')

# Load SDM grid
SDMClimate_grid <- read.csv('./STModel-Data/out_files/SDMClimate_grid.csv')

# Remove climatic spaces outside of the SDM calibration
selectedVars = c("annual_mean_temp", "tot_annual_pp", "mean_diurnal_range", "pp_warmest_quarter", "pp_wettest_period", "mean_temp_wettest_quarter", "mean_temp_driest_quarter")

# Get only variable selected
sdm_calib <- stateData[,selectedVars]
sdm_grid  <- SDMClimate_grid[,c("lat","lon",selectedVars)]

# Removed cells with temp>10°C
sdm_grid  <- subset(sdm_grid,annual_mean_temp<=10)

# Removed cells outside of the climatic space
for (i in 1:(ncol(sdm_calib))){
    rg <- range(sdm_calib[,i])
    sdm_grid <- sdm_grid[which(sdm_grid[,i+2]>=rg[1] & sdm_grid[,i+2]<=rg[2]),]
}

# Scales clim variables
for (i in 3:(ncol(sdm_grid))){
    col <- names(sdm_grid)[i]
    sdm_grid[,i] <- (sdm_grid[,i]-vars.means[col])/vars.sd[col]
}


# Grid prediction and Reshaping
pred_multinom <- predict(SDM1,new=sdm_grid,"prob")
pred_multinom <- data.frame(sdm_grid[,c("lat","lon")],pred_multinom,mod=rep("MN",rep=nrow(pred_multinom)))
pred_RF <- predict(SDM2,new=sdm_grid,"prob")
pred_RF <- data.frame(sdm_grid[,c("lat","lon")],pred_RF,mod=rep("RF",rep=nrow(pred_RF)))
pred <- rbind(pred_RF,pred_multinom)

df.pred <- melt(pred,c("lat","lon","mod"),value.name = "prob",variable.name = "state")
df.pred$mod <- as.factor(df.pred$mod)
df.pred$prob <- cut(df.pred$prob,breaks=11)
df.pred$prob <-factor(df.pred$prob ,levels=rev(levels(df.pred$prob)))

# Save Projs
save(pred,file="./data/sdm_proj.rdata")


# Crop lakes and countries on the area
lakes <- readOGR(dsn="./STModel-Data/out_files/shapefiles/",layer="lakes_stm_area")
countries <- readOGR(dsn="./STModel-Data/out_files/shapefiles/",layer="countries_stm_area")

ext <- extent(c(range(sdm_grid$lon),range(sdm_grid$lat)))

lakes <- crop(lakes,ext)
countries <- crop(countries,ext)

df.countries <- fortify(countries)
df.lakes <- fortify(lakes)


###########################################################################
## Maps

## set ggplot2 theme
theme_set(theme_grey(base_size=14))

cols <- brewer.pal(11,"Spectral")

map <- ggplot(df.pred) +
        geom_polygon(data = df.countries, aes(x = long, y = lat, group = group),fill="grey80",colour="grey50",size=0.1) +
        geom_raster(aes(lon,lat,fill=prob)) +
        facet_grid(state~mod)+
        scale_fill_manual(values=cols,name="Probability") +
        geom_polygon(data = subset(df.lakes,hole==FALSE),
            aes(x = long, y = lat, group = group),fill="lightskyblue",
            colour="dodgerblue4",size=0.1) +
        scale_x_continuous(expand=c(0,0))+
        scale_y_continuous(expand=c(0,0))+
        coord_equal() +
        xlab("Longitude") + ylab("Latitude")

ggsave(map,file="./figures/proj_SDM.jpg",width=8,height=10)


map <- ggplot(subset(df.pred,mod=='RF')) +
        geom_polygon(data = df.countries, aes(x = long, y = lat, group = group),fill="grey80",colour="grey50",size=0.1) +
        geom_raster(aes(lon,lat,fill=prob)) +
        facet_wrap(~state)+
        scale_fill_manual(values=cols,name="Probability") +
        geom_polygon(data = subset(df.lakes,hole==FALSE),
            aes(x = long, y = lat, group = group),fill="lightskyblue",
            colour="dodgerblue4",size=0.1) +
        scale_x_continuous(expand=c(0,0))+
        scale_y_continuous(expand=c(0,0))+
        coord_equal() +
        xlab("Longitude") + ylab("Latitude")

ggsave(map,file="./figures/RF_proj_SDM.jpg",width=15,height=10)

map <- ggplot(subset(df.pred,mod=='MN')) +
        geom_polygon(data = df.countries, aes(x = long, y = lat, group = group),fill="grey80",colour="grey50",size=0.1) +
        geom_raster(aes(lon,lat,fill=prob)) +
        facet_wrap(~state)+
        scale_fill_manual(values=cols,name="Probability") +
        geom_polygon(data = subset(df.lakes,hole==FALSE),
            aes(x = long, y = lat, group = group),fill="lightskyblue",
            colour="dodgerblue4",size=0.1) +
        scale_x_continuous(expand=c(0,0))+
        scale_y_continuous(expand=c(0,0))+
        coord_equal() +
        xlab("Longitude") + ylab("Latitude")

ggsave(map,file="./figures/MN_proj_SDM.jpg",width=15,height=10)

################################################
###### Explore SDMs responses to climate variables

require(dplyr)

desc_val<- sdm_grid[,selectedVars]
n <- 1000

out_ls <- list()

for (i in 1:ncol(desc_val)){
    var_test <- seq(min(desc_val[,i]),max(desc_val[,i]),length.out=n)
    vars_mean <- apply(desc_val,2,median)
    df <- data.frame(
        annual_mean_temp=rep(vars_mean[1],length(var_test)),
        tot_annual_pp=rep(vars_mean[2],length(var_test)),
        mean_diurnal_range=rep(vars_mean[3],length(var_test)),
        pp_warmest_quarter=rep(vars_mean[4],length(var_test)),
        pp_wettest_period=rep(vars_mean[5],length(var_test)),
        mean_temp_wettest_quarter=rep(vars_mean[6],length(var_test)),
        mean_temp_driest_quarter=rep(vars_mean[7],length(var_test)))
    df[,i] <- var_test

    pred_multinom <- predict(SDM1,new=df,"prob")
    df_multinom <- data.frame(model=rep("MN",nrow(pred_multinom)),var_test=rep(names(vars_mean)[i],nrow(pred_multinom)),value_var_test=var_test,pred_multinom)
    pred_RF <- predict(SDM2,new=df,"prob")
    df_RF <- data.frame(model=rep("RF",nrow(pred_multinom)),var_test=rep(names(vars_mean)[i],nrow(pred_multinom)),value_var_test=var_test,pred_RF)

    final_df <- rbind(df_multinom,df_RF)

    out_ls[[i]] <- final_df
}

ggdata <- melt(do.call(rbind,out_ls),id=c("model","var_test","value_var_test"),value.name="probability",variable.name="state")

ggplot(subset(ggdata,model=="MN"),aes(x=value_var_test,y=probability,colour=state)) + geom_line() + facet_wrap(~var_test,scales="free_x") + xlab("Var tested") + ylab("Probability")
ggsave(file="./figures/MN_oneVar_test.jpg",width=12,height=8)

ggplot(subset(ggdata,model=="RF"),aes(x=value_var_test,y=probability,colour=state)) + geom_line() + facet_wrap(~var_test,scales="free_x") + xlab("Var tested") + ylab("Probability")
ggsave(file="./figures/RF_oneVar_test.jpg",width=12,height=8)


