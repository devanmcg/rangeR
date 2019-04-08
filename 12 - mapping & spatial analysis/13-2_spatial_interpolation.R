install.packages(c("ggmap", "plyr", "akima"))

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
  (gg1 + geom_polygon(data=nd.counties, aes(x=long, y=lat, group=group), 
               color="grey60", fill=NA, size=0.75) + 
      geom_polygon(data=nd, aes(x=long, y=lat, group=group), 
               color="black", fill=NA, size=0.75) )

  library(plyr)
  count(ndawn.d, vars=c("Year", "Month", "Day"))
  
  last.week <- subset(ndawn.d, Month==11 & Day==14)

  (gg2 <- ggplot()  + coord_map() + theme_bw() + 
    geom_polygon(data=nd.counties, aes(x=long, y=lat, group=group), 
                 color="grey60", fill=NA, size=0.75) + 
    geom_polygon(data=nd, aes(x=long, y=lat, group=group), 
                 color="black", fill=NA, size=0.75) +
    geom_point(data=last.week, aes(x=Longitude, y=Latitude, 
                                   colour=Max.Temp), size=4) )
    gg2 + scale_colour_gradient(low = "blue", high = "red")  
                              
# Interpolate
  library(akima)
  interpTemp <- with(last.week, interp(x=Longitude, y=Latitude, 
                                        z=Max.Temp))
  interpTemp.df <- as.data.frame(interp2xyz(interpTemp))
  names(interpTemp.df) <- c("long", "lat", "max.temp")

# Map interpolated temperature
  (gg3 <- ggplot()  + coord_map() + theme_bw() +
          geom_tile(data=interpTemp.df, aes(x=long, y=lat, 
                                      fill=max.temp)) ) 
  gg3 + scale_fill_gradient(low = "blue", high = "red") 
  
  gg3 + geom_polygon(data=nd.counties, aes(x=long, y=lat, group=group), 
                         color="grey60", fill=NA, size=0.75) + 
    geom_polygon(data=nd, aes(x=long, y=lat, group=group), 
                 color="black", fill=NA, size=0.75) +
    geom_point(data=last.week, aes(x=Longitude, y=Latitude), size=3)  +
    scale_fill_gradient(low = "blue", high = "red") 
  
# Look at trends over time 
   fall.monthly <- ddply(ndawn.d, .(Station.Name,Latitude, Longitude, Month), 
                       summarize, max=max(Max.Temp))
   
# Semi-automatic interpolation:
    interpolator <- function(x) {
        interp <- with(x, interp(x=Longitude, y=Latitude, 
                                            z=max))
       interp.df <- as.data.frame(interp2xyz(interp))
       names(interp.df) <- c("long", "lat", "max.temp")
       return(interp.df) }
    
   monthly.interps <- rbind(data.frame(month="August", 
                     interpolator(subset(fall.monthly, Month==8) )), 
          data.frame(month="September", 
                      interpolator(subset(fall.monthly, Month==9) )), 
          data.frame(month="October", 
                     interpolator(subset(fall.monthly, Month==10) )),
          data.frame(month="November", 
                     interpolator(subset(fall.monthly, Month==11) )) ) 
    

    ggplot()  + coord_map() + theme_bw() +
      geom_tile(data=monthly.interps, aes(x=long, y=lat, 
                                        fill=max.temp)) +
      geom_polygon(data=nd.counties, aes(x=long, y=lat, group=group), 
                   color="grey60", fill=NA, size=0.75) + 
      geom_polygon(data=nd, aes(x=long, y=lat, group=group), 
               color="black", fill=NA, size=0.75) +
      geom_point(data=ndawn.d, 
                 aes(x=Longitude, y=Latitude), size=3)  +
      scale_fill_gradient(low = "blue", high = "red", 
                          na.value="transparent") +
         facet_wrap(~month)


