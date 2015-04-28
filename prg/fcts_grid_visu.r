
### Function to visualize prob map
require(ggplot2)
require(gridExtra)
require(sp)
require(ggthemes)

col_b <- "#189494"
col_t <- "#FEAC18"
col_r <- "tomato"
col_m <- "#87D187"

pal_state <- c(col_b,col_m,col_r,col_t)

theme_df <- theme_bw() + 
theme(
    plot.title = element_text(lineheight=.8),
    panel.margin = unit(c(0,0,0,0),"in"),
    plot.margin = unit(c(0,0,0,0),"in"),
    strip.text.x = element_text(size = 16,face="bold",colour = "white"),
    strip.background = element_rect(colour="grey10", fill="grey10"),
    panel.background = element_rect(fill = "light blue"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank())    



visu_prob_map <- function(data,pal,title,legend) {

    require(RColorBrewer)


    if(exists("df.countries") == FALSE & exists("df.lakes") == FALSE){
        load("./data/shapeFiles_forcrop.rdata")
    }

    cols <- brewer.pal(nlevels(data$prob),pal)

    ggplot(data,aes(x=lon,y=lat,fill=prob)) +
    facet_wrap(~state)+
    geom_polygon(data = df.countries, aes(x = long, y = lat, group = group),
        fill="grey80",colour="grey50",size=0.1) +
    geom_raster(alpha=0.7) +
    scale_fill_manual(values=cols,name=legend) +
    geom_polygon(data = subset(df.great_lakes,hole==FALSE),
        aes(x = long, y = lat, group = group),fill="light blue",
        colour="dodgerblue4",size=0.1) + theme_df +
    scale_x_continuous(expand=c(0,0))+
    scale_y_continuous(expand=c(0,0))+
    coord_equal() +
    xlab("Longitude") + ylab("Latitude")

}


visu_state_map <- function(data,legend=legend) {

    #if(exists("df.countries") == FALSE & exists("df.lakes") == FALSE){ 
    #    load("./data/shp_stm_area.rdata")
    #}

    ggplot(data=data,aes(x=lon,y=lat)) +
    geom_polygon(data = df.countries, aes(x = long, y = lat, group = group),
        colour=NA,size=0.2, fill="grey25") +
    geom_raster(aes(fill=state)) +
    scale_fill_manual(values=pal_state,name=legend) +
    geom_polygon(data = df.countries, aes(x = long, y = lat, group = group),
        colour="grey25",size=0.2, fill=NA) +
    geom_polygon(data = subset(df.lakes,hole==FALSE),
        aes(x = long, y = lat, group = group),fill="light blue",
        colour="dodgerblue4",size=0.1) + theme_df +
    scale_x_continuous(expand=c(0,0))+
    scale_y_continuous(expand=c(0,0))+
    coord_equal() +
    xlab("Longitude") + ylab("Latitude")
}