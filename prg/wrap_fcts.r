#### Functions managing simulation/outputs/Analyses of the STModel.
#### See http://github.com/QUICC-FOR/STModel-Simulation


#### Function to rebuild string 
################################################################################################

stmodel <- function(params=params,inland=inland,outland=outland,x=x,y=y,a=a,b=b,clim_file=clim_file,writeStep=writeStep,disturb=disturb,transProb=transProb){

    stmodel <- paste("./prg/stmodel -x",  x ,"-y",  y ,"-a",  a, "-b",  b, "-s -c", clim_file, " -p",paste("./pars/",params,sep=""),"-i",inland, "-t", writeStep, "-d", disturb,"-e",transProb, ">", outland, "2>/dev/null")

    system(stmodel)
}

##########################################################################################

runClimSimu <- function(params=params,grain=grain,timeSteps=timeSteps,transProb=transProb,writeStep=writeStep,disturb=disturb,clim_file=clim_file,land_file=land_file){

    x=y=a=b=grain

    #### Create time step sequence
    steps <- seq(writeStep,timeSteps,writeStep)

    #### Simulation Loop

    for (i in 1:length(steps)){
        if (i == 1){
            stmodel(params=params,inland=land_file,outland=paste('./temp_files/',steps[i],'step_',params,sep=""),x,y,a,b,clim_file,writeStep,disturb,transProb)
        } else {
            stmodel(params=params,inland=paste('./temp_files/',steps[i-1],'step_',params,sep=""),outland=paste('./temp_files/',steps[i],'step_',params,sep=""),x,y,a,b,clim_file,writeStep,disturb,transProb)
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
    clim <- read.csv(clim_file)
    clim$coords <- paste(clim$x,clim$y,sep="-")
    clim <- clim[,-3]
    names(clim)[3:4] <- c("annual_mean_temp","tot_annual_pp")
    clim$annual_mean_temp <- clim$annual_mean_temp * vars.sd['annual_mean_temp'] + vars.means['annual_mean_temp']
    clim$tot_annual_pp <- clim$tot_annual_pp * vars.sd['tot_annual_pp'] + vars.means['tot_annual_pp']

    #### Manage outputs to rasterbrick

    for(i in 1:length(temp_df$path)){

        dat <- read.csv(temp_df$path[i],header=FALSE,stringsAsFactors=FALSE)
        names(dat) <- c("x","y","state")

        dat[which(dat$state=="B"),"state"] <- "1"
        dat[which(dat$state=="T"),"state"] <- "2"
        dat[which(dat$state=="M"),"state"] <- "3"
        dat[which(dat$state=="R"),"state"] <- "4"
        dat[which(dat$state=="0"),"state"] <- NA
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

    return(1)
}

##########################################################################################

getClimFigs <- function(simu_out){

    load(paste("./outputs/",simu_out,sep=""))

    # PrepData
    ##############
    ## Count states over time
    count <- lapply(ls_rs,table)
    count <- do.call(rbind,count)
    count <- count/rowSums(count)
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
    final_df$state <- as.factor(final_df$state)

    # Manage level order
    df_count$state <- factor(df_count$state,levels=levels(final_df$state),labels=levels(final_df$state))


    ### Figures

    co_time <- ggplot(df_count,aes(x=step,y=count,group=state,color=state))+geom_line(size=0.5) + scale_colour_brewer(palette="Accent",name="State") + xlab("Timestep") + ylab("State proportion on the landscape") + ggtitle("State proportion over time") + theme_few()

    land_final <- ggplot(final_df,aes(x=tot_annual_pp,y=annual_mean_temp,fill=state))+ geom_raster() + scale_fill_brewer(palette="Accent",name="State")+ xlab("Annual mean temp (°C)") + ylab("Precipitation (mm)") + scale_x_continuous(expand=c(0,0))+scale_y_continuous(expand=c(0,0))+ ggtitle("Final landscape") +theme_few() 

    g <- arrangeGrob(co_time,land_final)

    dev.off()
    
    ggsave(paste("figures/",strsplit(simu_out, "\\.")[[1]][1],".pdf",sep=""),g,dpi=300,width=8)

    dev.off()
    
    return(1)
}

runGeoSimu <- function(params=params,timeSteps=timeSteps,transProb=transProb,writeStep=writeStep,disturb=disturb,clim_file=clim_file,land_file=land_file){

    load("./data/inputGeoSimu.rdata")

    x=max(init_grid$x)+1
    y=max(init_grid$y)+1
    a=max(geoClimGrid$x)+1
    b=max(geoClimGrid$y)+1

    #### Create time step sequence
    steps <- seq(writeStep,timeSteps,writeStep)

    #### Simulation Loop

    for (i in 1:length(steps)){
        if (i == 1){
            stmodel(params=params,inland=inland,outland=paste('./temp_files/',steps[i],'_step_',params,sep=""),x=x,y=y,a=a,b=b,clim_file=clim_file,writeStep=writeStep,disturb=disturb,transProb=transProb)
        } else {
            stmodel(params=params,inland=paste('./temp_files/',steps[i-1],'_step_',params,sep=""),outland=paste('./temp_files/',steps[i],'_step_',params,sep=""),x=x,y=y,a=a,b=b,clim_file=clim_file,writeStep=writeStep,disturb=disturb,transProb=transProb)
        }
    }

    #### Outputs
    ################################################################################################

    vec_path <- list.files("./temp_files/",full.names=TRUE)
    vec_file <- list.files("./temp_files/")

    temp_df <- data.frame(path=vec_path,file=vec_file,step=as.numeric(gsub("([0-9]+).*$", "\\1", vec_file)),stringsAsFactors=FALSE)
    temp_df <- temp_df[order(temp_df$step),]
    ls_rs <- list()


    #### Manage outputs to rasterbrick

    for(i in 1:length(temp_df$path)){
        dat <- read.csv(temp_df$path[i],header=FALSE,stringsAsFactors=FALSE)
        names(dat) <- c("x","y","state")

        dat[which(dat$state=="B"),"state"] <- "1"
        dat[which(dat$state=="T"),"state"] <- "2"
        dat[which(dat$state=="M"),"state"] <- "3"
        dat[which(dat$state=="R"),"state"] <- "4"
        dat[which(dat$state=="0"),"state"] <- NA
        dat$state <- as.numeric(dat$state)  


        ### Create matrix
        dat <- acast(dat,rev(y)~x,value.var = "state")
        rs <- raster(dat)
        proj4string(rs) <- CRS("+init=epsg:4269 +proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs +towgs84=0,0,0")
        extent(rs) <- c(range(geoCoordsGrid$lon),range(geoCoordsGrid$lat))

        ### store matrix
        ls_rs[[i]] <- rs

        file.remove(temp_df$path[i])
    }

    names(ls_rs) <- temp_df$step
    st_rs <- stack(ls_rs)
    save(st_rs,file=paste("./outputs/",paste("GEO_",strsplit(params, "\\.")[[1]][1],strsplit(params, "\\.")[[1]][2],sep=""),"_tp_",timeSteps,".rdata",sep=""))

    return(1)

}