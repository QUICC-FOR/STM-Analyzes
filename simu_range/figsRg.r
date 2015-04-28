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
load('./outputs/RG_eqLand_fakeClim_rep_10_GenSA_rf_0_ts_10_1000.rdata')
clim <- read.csv("./simu_range/init_fakeClim_wt_cc.csv")
clim$coords <- paste(clim$x,clim$y,sep="-")

dat <- as.data.frame(st_rs,xy=TRUE,na.rm=TRUE)
names(dat)[1:2] <- c("lon","lat") 
dat<- melt(dat,id=c("lon","lat"),value.name="state",variable.name="step")

vec_chr <- as.character(dat$step)
vec_chr<- do.call(rbind,strsplit(vec_chr, "rep"))
vec_rep <- str_extract(vec_chr[,2],"[[:digit:]]+")
vec_ts <-  str_extract(vec_chr[,1],"[[:digit:]]+")

dat$step <- vec_ts
dat$rep <- vec_rep
dat$step <- as.numeric(dat$step)
dat$rep <- as.numeric(dat$rep)
dat$state <- idToState(dat$state)
dat$state <- as.factor(dat$state)

#Transfo lat/long to x/y
dat$x <- as.numeric(as.factor(dat$lon))-1
dat$y <- as.numeric(as.factor(dat$lat))-1
dat$coords <- paste(dat$x,dat$y,sep="-")

datClim <- merge(clim,dat,by="coords")
datClim <- datClim[,-c(2:4,dim(datClim)[2]-1,dim(datClim)[2])]


#Detransfo clim
load("./data/scale_info.Robj")
names(datClim)[2:3] <- c("annual_mean_temp","tot_annual_pp")
datClim$annual_mean_temp <- datClim$annual_mean_temp * vars.sd['annual_mean_temp'] + vars.means['annual_mean_temp']
datClim$tot_annual_pp <- datClim$tot_annual_pp * vars.sd['tot_annual_pp'] + vars.means['tot_annual_pp']

# Import actual landscape
land <- read.csv("./simu_range/init_eq_landRgGrid.csv",header=FALSE)
names(land) <- c("x","y","state")
land$rep <- 1
land$step<- 0
land$coords <- paste(land$x,land$y,sep="-")
landClim <- merge(unique(datClim[,c("annual_mean_temp","tot_annual_pp","lon","lat","coords")]),land,by="coords")
landClim <- landClim[,-c(6:7)]

# prep data
datClim <- rbind(datClim,landClim)
datClim <- datClim[,-1]
datClim$step <- as.factor(datClim$step)
datClim$rep <- as.factor(datClim$rep)

## transfo
require(dplyr)
prepDat <- datClim %>% group_by(step,annual_mean_temp,rep,state) %>% summarise(count = n())
prop_TM <- dcast(prepDat, step+annual_mean_temp+rep ~ state,value.var="count",fill=0)
prop_TM$total <- rowSums(prop_TM[,c("T","M","B")])
prop_TM$prop <- rowSums(prop_TM[,c("T","M")]) / prop_TM$total
prop_TM <- prop_TM[,c("step","annual_mean_temp","rep","prop")]
prop_TM <- prop_TM %>% group_by(step,annual_mean_temp) %>% summarise(avg_prop=mean(prop))

prop_TM$step <- factor(prop_TM$step,levels=rev(levels(prop_TM$step)),labels=rev(levels(prop_TM$step)))

ggplot(subset(prop_TM,step=="20"|step=="0"|step=="100"|step=="200"|step=="1000"),aes(x=annual_mean_temp,y=avg_prop,group=step)) + 
geom_ribbon(aes(ymin=0,ymax=avg_prop,fill=step))+  geom_line(color="grey25",size=0.3) +
scale_fill_brewer(palette="YlOrBr",name="Timesteps ")+
geom_vline(xintercept=7, colour="white", linetype = "longdash") +
geom_text(aes(x=7, label="Montréal", y=0.5), colour="white", angle=90, vjust = -1, text=element_text(size=12))+
geom_vline(xintercept=0.5, colour="grey30", linetype = "longdash") +
geom_text(aes(x=0.5, label="Chibougamau", y=0.5), colour="grey30", angle=90, vjust = -1, text=element_text(size=16))+
scale_x_reverse(expand=c(0,0),breaks=seq(-1,8,1))+
scale_y_continuous(expand=c(0,0))+
xlab("Annual mean temperature (°C)")+
ylab("Proportion of temperate states (T+M)")+
theme_bw(18)+
theme(legend.position="bottom") 
#}

ggsave("./simu_range/fut_range.pdf",height=8,width=16)  