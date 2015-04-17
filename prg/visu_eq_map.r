## Visu equilibrium maps

setwd("~/Documents/GitHub/STModel-Analyzes/")
source("./prg/wrap_fcts.r")
source("./fcts_grid_visu.r")

require(raster)
require(rgdal)

# Prep shp cover
#load("./data/shp_stm_area.rdata")

ls_out <- c("GEO_GenSA_initForFit_rf_03315y2_tp_1000.rdata","GEO_GenSA_initForFit_rf_03315y_tp_1000.rdata","GEO_GenSA_initForFit_rf_03322_tp_1000.rdata","GEO_GenSA_initForFit_rf_03325y_tp_1000.rdata","GEO_GenSA_initForFit_rf_03332_tp_1000.rdata","GEO_GenSA_initForFit_rf_03335y2_tp_1000.rdata","GEO_GenSA_initForFit_rf_03335y_tp_1000.rdata","GEO_GenSA_initForFit_rf_0333_tp_1000.rdata","GEO_GenSA_initForFit_rf_03345y_tp_1000.rdata","GEO_GenSA_initForFit_rf_03352_tp_1000.rdata","GEO_GenSA_initForFit_rf_03355y_tp_1000.rdata","GEO_GenSA_initForFit_rf_0335_tp_1000.rdata","GEO_GenSA_initForFit_rf_03365y_tp_1000.rdata","GEO_GenSA_initForFit_rf_03375y_tp_1000.rdata","GEO_GenSA_initForFit_rf_03385y_tp_1000.rdata")

for (i in 1:length(ls_out)){

    out <- ls_out[i]
    load(paste("./outputs/",out,sep=""))

    # Import last step
    df_out <- as.data.frame(st_rs[[nlayers(st_rs)]],xy=TRUE)
    names(df_out) <- c("lon","lat","state")

    # Reverse transfo
    df_out[which(df_out$state==1),"state"] <- "B"
    df_out[which(df_out$state==2),"state"] <- "T"
    df_out[which(df_out$state==3),"state"] <- "M"
    df_out[which(df_out$state==4),"state"] <- "R"
    df_out$state <- as.factor(df_out$state)

    visu_state_map(df_out,title=out,"State") 
    ggsave(paste("./figures/",strsplit(out,"\\.")[[1]][1],"_diststate.jpg",sep=""),height=6,width=10)
}