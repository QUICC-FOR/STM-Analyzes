#### This R program is a wrapper managing simulation/outputs/Analyses of the STModel.
#### See http://github.com/QUICC-FOR/STModel-Simulation

rm(list=ls())

setwd("~/Documents/GitHub/STModel-Analyzes/")

#### Librairy
require(reshape2)
require(raster)
require(ggplot2)

##### Init model pars
##############################################################################################

grain=100
params='GenSA_initForFit_rf_0.339.txt'
timeSteps=10000

## Constant
disturb= 0.0
transProb=1
writeStep=10
clim_file="./data/init_clim.csv"
land_file="./data/init_M_land.csv"

#### Generate Input files
##############################################################################################

#### load transitions data
load("./data/transitions_r1.rdata")

transitionData <- subset(transitionData,annual_mean_temp<=10)
rg_tp <- range(transitionData$annual_mean_temp)
rg_pp <- range(transitionData$tot_annual_pp)

#### Scale range of clim variable
load("./data/scale_info.Robj")
rg_tp <- (rg_tp - vars.means['annual_mean_temp'])/vars.sd['annual_mean_temp']
rg_pp <- (rg_pp - vars.means['tot_annual_pp'])/vars.sd['tot_annual_pp']

#### Create initial landscape grid 
coord_x <- coord_y <- seq(0,grain-1,1)
coord_grid <- expand.grid(coord_x,coord_y)

land <- data.frame(x=coord_grid[,1],y=coord_grid[,2],state='M')
write.table(land,land_file,sep=",",quote=FALSE,row.names=FALSE,col.names=FALSE)

#### Create initial clim landscape
env1 <- seq(rg_tp[1],rg_tp[2],length.out=grain)
env2 <- seq(rg_pp[1],rg_pp[2],length.out=grain)
clim_grid <- expand.grid(env1,env2)

clim <- data.frame(x=coord_grid[,1],y=coord_grid[,2],year=0,env1=clim_grid[,1],env2=clim_grid[,2])
write.table(clim,clim_file,sep=",",row.names=FALSE)

#### Set Model flags
################################################################################################

# Optional command line options. Default values shown in parentheses:
#   -h: help; display this message
#   -x <int>: specify x dimension of the simulation grid; must divide evenly into x-dim of climate grid (100)
#   -y <int>: specify y dimension of the simulation grid; must divide evenly into x-dim of climate grid (100)
#   -a <int>: specify x dimension of the climate grid (10)
#   -b <int>: specify y dimension of the climate grid (10)
#   -c <filename>: specify the input climate datafile; must match values in -a, -b, and -t options (climate_test.csv)
#   -s: specify constant climate (instead of varying in time) (unset)
#   -p <filename>: specify a file for reading the parameters for the climate model (NULL)
#         format: %v%s%i ddddd
#         %v: the first letter of the variable name (e.g., a for alpha)
#         %s: the first letter of the state for the variable in question (e.g., b for boreal)
#         %i: the index of the variable; 0 for intercept, 1 for first term, etc
#         dddd: a floating point number with the parameter value
#   -i <filename>: use a previously output grid as input, rather than initializing a new grid (unset)
#   -t <int>: specify the number of time steps after after initial conditions to run the simulation (20)
#   -d <float>: specify initial disturbance rate between 0 and 0.600000 (0.200000)
#   -v: specify Von Neuman neighborhoods (4-cell; default is 8-cell Moore neighborhood)
#   -g: pick n cells randomly in the grid to compute prevalence (on the entire grid; default is local)
#   -e: Specify timesteps in each iteration (default:10)


#### Function to rebuild string 
################################################################################################
stmodel <- function(params,inland,outland){

    stmodel <- paste("./prg/stmodel -x",  grain ,"-y",  grain ,"-a",  grain, "-b",  grain, "-s -c", clim_file, " -p",paste("./pars/",params,sep=""),"-i",inland, "-t", writeStep, "-d", disturb,"-e",transProb, ">", outland,"2>/dev/null")

    system(stmodel)

}

#### Create time step sequence
steps <- seq(writeStep,timeSteps,writeStep)

#### Simulation Loop

for (i in 1:length(steps)){
    if (i == 1){
        stmodel(params=params,inland=land_file,outland=paste('./temp_files/',steps[i],'step_',params,sep=""))
    } else {
        stmodel(params=params,inland=paste('./temp_files/',steps[i-1],'step_',params,sep=""),outland=paste('./temp_files/',steps[i],'step_',params,sep=""))
    }
}

#### Outputs
################################################################################################

vec_path <- list.files("./temp_files/",full.names=TRUE)
vec_file <- list.files("./temp_files/")

temp_df <- data.frame(path=vec_path,file=vec_file,step=as.numeric(gsub("([0-9]+).*$", "\\1", vec_file)),stringsAsFactors=FALSE)
temp_df <- temp_df[order(temp_df$step),]
ls_rs <- list()

#### Rescale and prep to merge clim
clim$coords <- paste(clim$x,clim$y,sep="-")
clim <- clim[,-3]
names(clim)[3:4] <- c("annual_mean_temp","tot_annual_pp")
clim$annual_mean_temp <- clim$annual_mean_temp * vars.sd['annual_mean_temp'] + vars.means['annual_mean_temp']
clim$tot_annual_pp <- clim$tot_annual_pp * vars.sd['tot_annual_pp'] + vars.means['tot_annual_pp']

#### Manage outputs to rasterbrick

for(i in 1:length(temp_df$path)){

    dat <- read.csv(temp_df$path[i],header=FALSE,stringsAsFactors=FALSE)
    names(dat) <- c("x","y","state")

    dat$state[dat$state=="B"] <- "1"
    dat$state[dat$state=="T"] <- "2"
    dat$state[dat$state=="M"] <- "3"
    dat$state[dat$state=="R"] <- "4"
    dat$state[dat$state=="0"] <- NA
    dat$state <- as.numeric(dat$state)  
    dat$coords <- paste(dat$x,dat$y,sep="-")

    ### Link to climate
    dat <- merge(dat[,c(3:4)],clim,by="coords")

    ### Create matrix
    dat <- acast(dat,annual_mean_temp~tot_annual_pp,value.var = "state")

    ### store matrix
    ls_rs[[i]] <- dat

    file.remove(temp_df$path[i])
}

names(ls_rs) <- temp_df$step
save(ls_rs,file=paste("./outputs/",paste(strsplit(params, "\\.")[[1]][1],strsplit(params, "\\.")[[1]][2],sep=""),"_tp_",timeSteps,"_gr_",grain,".rdata",sep=""))

#### Figures 
################################################################################################

# output <- ""
# load(output)
# grain = 100

# PrepData
##############
## Count states over time
count <- lapply(ls_rs,table)
count <- do.call(rbind,count)/(grain*grain)
df_count <- data.frame(step=as.numeric(rownames(count)),B=count[,1],T=count[,2],M=count[,3],R=count[,4])
df_count <- melt(df_count,id=c("step"),value.name="count",variable.name="state")


## Final landscape
final_step <- length(ls_rs)
final_df <- melt(ls_rs[[as.character(final_step)]])
names(final_df) <- c("annual_mean_temp","tot_annual_pp","state")

final_df$state[final_df$state==1] <- "B"
final_df$state[final_df$state==2] <- "T"
final_df$state[final_df$state==3] <- "M"
final_df$state[final_df$state==4] <- "R"
final_df$state[final_df$state==0] <- NA
final_df$state <- as.character(final_df$state)

### Figures

co_time <- ggplot(df_count,aes(x=step,y=count,group=state,color=state)) + geom_line(size=1) + scale_colour_brewer(palette="Accent")
land_final <- ggplot(final_df,aes(x=tot_annual_pp,y=annual_mean_temp,fill=state)) + geom_raster() + scale_fill_brewer(palette="Accent")
