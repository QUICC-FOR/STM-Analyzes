### Landscape analysis functions

# Desc coming

bounds <- function(spdf,probs){

    states <- levels(spdf$state)

    ls.out <- list()

    for (i in 1:length(states)){
        st.spdf <- subset(spdf,state==states[i])
        lat <- melt(quantile(st.spdf[,1],probs),value.name='lat')
        lon <- melt(quantile(st.spdf[,2],probs),value.name='lon')
        ls.out[[i]] <- data.frame(lat=lat,lon=lon, perc=row.names(lat),state=states[i],row.names=NULL)
    }

    return(rbind.fill(ls.out))
}