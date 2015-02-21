
### Function to visualize prob map
require(ggplot2)
require(gridExtra)

theme_df <-  theme(
            plot.title = element_text(lineheight=.8),
            panel.margin = unit(c(0.5,0,0,0),"in"),
            plot.margin = unit(c(0.1,0,0,0),"in"),
            strip.text.x = element_text(size = 12,face="bold",colour = "white"),
            strip.background = element_rect(colour="black", fill="black"),
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
        geom_polygon(data = subset(df.lakes,hole==FALSE),
            aes(x = long, y = lat, group = group),fill="light blue",
            colour="dodgerblue4",size=0.1) +
        scale_x_continuous(expand=c(0,0))+
        scale_y_continuous(expand=c(0,0))+
        coord_equal() +
        xlab("Longitude") + ylab("Latitude")+
        ggtitle(title)+
        theme_df

}
