install.packages(pacman)
pacman::p_load(maps, ggplot2, ggmap, broom, rgdal, rgeos,plyr, maptools )

# Get started with maps
  library(maps)
  map() 
  map('usa')
  map('state')
  map('county')
  map('state', add=TRUE, lwd=2)
  
# Additional features in ggmap
  # Fetch coordinates of places by name
  library(ggmap)
  fargo <- geocode("Fargo, North Dakota", messsaging=FALSE) 
  # Add to map
  points(fargo, pch=21, bg="blue", col="white", cex=2)
  
  # zoom in
  map('county', 'north dakota', lwd=1, col="grey60")
  map('state', add=TRUE, lwd=2)
  points(fargo, pch=21, bg="blue", col="white", cex=2)
  
  nd.features <- rbind( data.frame(feature="city", name="Fargo", 
                                 geocode("Fargo, North Dakota")), 
                      data.frame(feature="city", name="Grand Forks", 
                                 geocode("Grand Forks, North Dakota")), 
                     data.frame(feature="city", name="Bismarck", 
                                geocode("Bismarck, North Dakota")),
                      data.frame(feature="city", name="Minot", 
                                 geocode("Minot, North Dakota")), 
                     data.frame(feature="rec", name="Hettinger", 
                                geocode("Hettinger, North Dakota")), 
                     data.frame(feature="rec", name="Williston", 
                                geocode("Williston, North Dakota")), 
                     data.frame(feature="rec", name="Carrington", 
                                geocode("Carrington, North Dakota")), 
                     data.frame(feature="rec", name="Dickinson", 
                                geocode("Dickinson, North Dakota")),
                     data.frame(feature="rec", name="Central Grasslands", 
                                geocode("Streeter, North Dakota")), 
                     data.frame(feature="rec", name="Langdon", 
                                geocode("Langdon, North Dakota")))
  
  with(subset(nd.features, feature=="city"), 
       points(lon, lat, pch=21, bg="blue", col="white", cex=2))
  with(subset(nd.features, feature=="city"), 
       text(lon, lat, pos=3, labels=name, col="blue", cex=1))
  with(subset(nd.features, feature=="rec"), 
       points(lon, lat, pch=24, bg="orange", col="white", cex=2))
  with(subset(nd.features, feature=="rec"), 
       text(lon, lat, pos=3, labels=name, col="black", cex=1))
  
# Better mapping with ggplot
  # Get data
    states.md <- map_data("state")
    l48 <- subset(states.md, region !="alaska")
  
  # Plot map with ggplot
    # note use of geom_polygon
    library(ggplot2)
    L48.gg <- ggplot() +
          geom_polygon(data=l48, aes(x=long, y=lat, group=group), 
                       color="white", fill="grey90", size=0.25)
    L48.gg 
    L48.gg + theme_bw() 
    L48.gg + theme_nothing() 
    (L48.gg <- L48.gg  + theme_bw() + coord_map() ) 
# Add spatial point data via geom_point:
L48.gg + geom_point(data=nd.features, aes(x=lon, y=lat, shape=feature, color=feature))
    
# Import spatial points data 
  str(fear.us)
  # Extract, combine relevant data for mapping
  fear.us@data$id <- rownames(fear.us@data)
  fear.us.pts <- data.frame(objectid=fear.us$OBJECTID, 
                            long=fear.us$coords.x1, 
                            lat=fear.us$coords.x2)
  
  # View extent of species distribution:
  L48.gg + geom_point(data=fear.us.pts, aes(x=long, y=lat)) 
  
  # Crop map to regional area of interest:
  L48.gg + geom_point(data=fear.us.pts, aes(x=long, y=lat)) +
    coord_fixed(1.3, xlim=c(-104, max(l48$long)), ylim=l48$lat)
  
# Import shapefile
  str(us.epa.III)
  # Messy!
  # And coordinate systems are off
  # spTransform converts to long/lat
  epa.regions <- us.epa.III
  epa.regions  <- spTransform(us.epa.III, CRS("+init=epsg:4326"))
  epa.regions@data$id <- rownames(epa.regions@data)
  epa.regions.points <- tidy(epa.regions, region="id")
  epa.regions.df <- join(epa.regions.points, epa.regions@data, by="id")
  
  # Map species distribution on top of ecoregions
  L48.gg + geom_polygon(data=epa.regions.df[epa.regions.df$NA_L1CODE %in% 
                                              c('8','9'),], 
                        aes(x=long, y=lat, group=group, 
                            alpha=0.5, fill=NA_L1NAME)) +
    scale_alpha_continuous(guide=FALSE) + 
    geom_point(data=fear.us.pts, aes(x=long, y=lat)) +
    coord_fixed(1.3, xlim=c(-104, max(l48$long)), ylim=l48$lat)
