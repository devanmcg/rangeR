install.packages(c("pacman"))
pacman::p_load(ggplot2, ggmap, plyr, lubridate, akima)

ndawn.d <- read.csv("./ndawn_fall_2016.csv")

head(ndawn.d)

# Is this a map? 
  (gg1 <-  ggplot()  + theme_bw() + 
          geom_point(data=ndawn.d, aes(x=Longitude, y=Latitude)) )

  (gg1 <- gg1 + coord_map() )

# Get ND base layer
  library(ggmap)
  nd <- subset(map_data("state"), region =="north dakota")
  nd.counties <- subset(map_data("county"), region =="north dakota")

# Combine stations with map
  (gg1 + geom_polygon(data=nd.counties, 
                      aes(x=long, y=lat, group=group), 
               color="grey60", fill=NA, size=0.75) + 
        geom_path(data=nd, 
                     aes(x=long, y=lat, group=group), 
                 color="black", size=0.75) )

  library(plyr)
  plyr::count(ndawn.d, vars=c("Year", "Month", "Day"))
  
   one.week <- subset(ndawn.d, Month==11 & Day==14)

  (gg2 <- ggplot()  + coord_map() + theme_bw() + 
    geom_polygon(data=nd.counties, aes(x=long, y=lat, group=group), 
                 color="grey60", fill=NA, size=0.75) + 
    geom_polygon(data=nd, aes(x=long, y=lat, group=group), 
                 color="black", fill=NA, size=0.75) +
    geom_point(data=one.week, aes(x=Longitude, y=Latitude, 
                                   colour=MaxTemp), size=4) )
    gg2 + scale_colour_gradient(low = "blue", high = "red")  
                              
# Interpolate
  library(akima)
  interpTemp <- with(one.week, interp(x=Longitude, y=Latitude, 
                                        z=MaxTemp))
  interpTemp.df <- as.data.frame(interp2xyz(interpTemp))
  names(interpTemp.df) <- c("long", "lat", "MaxTemp")

# Map interpolated temperature
  (gg3 <- ggplot()  + coord_map() + theme_bw() +
          geom_tile(data=interpTemp.df, aes(x=long, y=lat, 
                                      fill=MaxTemp)) ) 
  gg3 + scale_fill_gradient(low = "blue", high = "red") 
  
  gg3 + geom_path(data=nd.counties, 
                  aes(x=long, y=lat, group=group), 
                     color="grey60", size=0.75) + 
    geom_polygon(data=nd, aes(x=long, y=lat, group=group), 
                 color="black", fill=NA, size=0.75) +
    geom_point(data=one.week, aes(x=Longitude, y=Latitude), size=3)  +
    scale_fill_gradient(low = "blue", high = "red") 
  
# Look at trends over time 
  # Summarize max temps by month
  ndawn.d<- ndawn.d %>%
    mutate(Month = lubridate::month(Month, 
                                    label=TRUE, 
                                    abbr=FALSE))
  
   fall.monthly <- ddply(ndawn.d, .(StationName,Latitude, 
                                    Longitude, Month), 
                        summarize, 
                        max=max(MaxTemp)) 
                  
   
  # Get interpolations for each month w/ custom function (see for loop alternative below)
    monthly.interps <- ddply(fall.monthly, 
                              .(Month), 
                             function(x) {
                               interp <- with(x, interp(x=Longitude, 
                                                        y=Latitude, 
                                                        z=max))
                               interp.df <- as.data.frame(interp2xyz(interp))
                               names(interp.df) <- c("long", "lat", "MaxTemp")
                               return(interp.df) } ) 
  # Plot
    ggplot()  + coord_map() + theme_bw() +
      geom_tile(data=monthly.interps, 
                aes(x=long, y=lat, fill=MaxTemp)) +
      geom_path(data=nd.counties, 
                aes(x=long, y=lat, group=group), 
                color="grey60", size=0.75) + 
      geom_path(data=nd, 
                aes(x=long, y=lat, group=group), 
               color="black", size=0.75) +
      geom_point(data=ndawn.d, 
                 aes(x=Longitude, y=Latitude), size=3)  +
      scale_fill_gradient(low = "blue", high = "red", 
                          na.value="transparent", 
                          name="Monthly Max Temp") +
      theme(legend.position = "top") +
      facet_wrap(~Month)
    
    
# An alternative to ddply using a for loop
    # Note vectorized solutions like plyr or dplyr functions 
    # are considered better for programming in R, 
    # but sometimes for loops are more intuitive to write & easier to operate 
    
    monthly.interps <- data.frame() 
    
    for(i in length(unique(fall.monthly$Month))) {
      i = 1 
      M = unique(fall.monthly$Month)[1]
      d = subset(fall.monthly, Month == M)
      interp <- with(d, interp(x=Longitude, 
                               y=Latitude, 
                               z=max))
      mon.int <- as.data.frame(interp2xyz(interp))
      names(mon.int) <- c("long", "lat", "MaxTemp")
      monthly.interps <- rbind(monthly.interps, mon.int)
    }