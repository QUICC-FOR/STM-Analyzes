## Prep 
setwd("~/Documents/GitHub/STModel-Analyzes/")
rm(list=ls())

### set workspace 
source("./prg/wrap_fcts.r")
source("./prg/fcts_grid_visu.r")

### Required libraries 
require(raster)
require(rgdal)
require(ggplot2)
require(stringr)
require(reshape2)

### Load stack and clim
load('./outputs/RG_realClim_rep_1_GenSA_rf_0_ts_10_5000.rdata')
clim <- read.csv("./simu_range/init_clim_wt_cc.csv")
clim$coords <- paste(clim$x,clim$y,sep="-")

dat <- as.data.frame(st_rs,xy=TRUE,na.rm=TRUE)
names(dat)[1:2] <- c("lon","lat") 
dat<- melt(dat,id=c("lon","lat"),value.name="state",variable.name="step")

vec_chr <- as.character(dat$step)
vec_chr<- do.call(rbind,strsplit(vec_chr, "rep"))
vec_rep <- str_extract(vec_chr[,2],"[[:digit:]]+")
vec_ts <-  str_extract(vec_chr[,1],"[[:digit:]]+")

dat$step <- as.numeric(vec_ts)
dat$rep <- as.numeric(vec_rep)
dat$step <- as.factor(dat$step)
dat$step <- factor(dat$step,levels=as.numeric(levels(dat$step))[order(as.numeric(levels(dat$step)))])
dat$rep <- as.factor(dat$rep)
dat$state <- idToState(dat$state)
dat$state <- as.factor(dat$state)


#Transfo lat/long to x/y
dat$x <- as.numeric(as.factor(dat$lon))-1
dat$y <- as.numeric(as.factor(dat$lat))-1
dat$coords <- paste(dat$x,dat$y,sep="-")

datClim <- merge(clim,dat,by="coords")
datClim <- datClim[,-c(1:4,dim(datClim)[2]-1,dim(datClim)[2])]
datClim$state <- droplevels(datClim$state)

#Detransfo clim
load("./data/scale_info.Robj")
names(datClim)[1:2] <- c("annual_mean_temp","tot_annual_pp")
datClim$annual_mean_temp <- datClim$annual_mean_temp * vars.sd['annual_mean_temp'] + vars.means['annual_mean_temp']
datClim$tot_annual_pp <- datClim$tot_annual_pp * vars.sd['tot_annual_pp'] + vars.means['tot_annual_pp']


## transfo
require(dplyr)
require(RColorBrewer)
prepDat <- datClim %>% group_by(step,lat,rep,state) %>% summarise(count = n())
prop_TM <- dcast(prepDat, step+lat+rep ~ state,value.var="count",fill=0)
prop_TM$lat <- (prop_TM$lat - min(prop_TM$lat))/1000
prop_TM$total <- rowSums(prop_TM[,c("T","M","B","R")])
prop_TM$prop <- rowSums(prop_TM[,c("T","M")]) / prop_TM$total
prop_TM <- prop_TM[,c("step","lat","rep","prop")]
prop_TM <- prop_TM %>% group_by(step,lat) %>% summarise(avg_prop=mean(prop))

prop_TM$step <- factor(prop_TM$step,levels=rev(levels(prop_TM$step)),labels=rev(levels(prop_TM$step)))
prop_TM <- subset(prop_TM,step=="20"|step=="0"|step=="100"|step=="200"|step=="1000")
prop_TM$step <- droplevels(prop_TM$step) 

ggplot(prop_TM,aes(x=lat,y=avg_prop,group=step)) + 
geom_ribbon(aes(ymin=0,ymax=avg_prop,fill=step,group=step))+  
geom_line(aes(x=lat,y=avg_prop,group=step),color="grey15",size=0.3) +
scale_fill_manual(values = rev(brewer.pal(5,"YlOrBr")),name="Timesteps ")+
geom_vline(xintercept=15, colour="grey25", linetype = "longdash") +
geom_text(aes(x=15, label="Montréal", y=0.25), colour="grey25", hjust = -0.3, text=element_text(size=12))+
geom_vline(xintercept=500, colour="grey25", linetype = "longdash") +
geom_text(aes(x=500, label="Chibougamau", y=0.25), colour="grey25",  hjust = 1.2, text=element_text(size=16))+
scale_x_continuous(expand=c(0,0),breaks=c(seq(0,500,20)))+
scale_y_continuous(expand=c(0,0))+
xlab("Distance (km)")+
ylab("Proportion of temperate states (T+M)")+
theme_bw(18)+
theme(legend.position="bottom") 
#}

ggsave("./simu_range/fut_range.pdf",height=8,width=16)  