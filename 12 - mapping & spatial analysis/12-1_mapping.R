install.packages(pacman)
pacman::p_load(maps, plyr, tidyverse, ggmap, broom, rgdal, rgeos, maptools, sf, viridis )

source('https://raw.githubusercontent.com/devanmcg/rangeR/master/R/CustomGGplotThemes.R')

# Load the three .Rdata files
    # nd.features.d
    # fear.us
    # us.epa.III

# Get started with maps
  # maps package gives us maps for base graphics 
  # Note that map() function conflicts with purrr package from tidyverse
  maps::map() 
  par(mar=c(4, 2, 4, 2))
  maps::map('usa')
  maps::map('state')
  maps::map('county')
  maps::map('state', add=TRUE, lwd=2)
  
  # Add points to map
  points(subset(nd.features.d, name=="Fargo")[4:5],
         pch=21, bg="blue", col="white", cex=2)
  
  # zoom in
  par(mar=c(4, 2, 4, 2))
  maps::map('county', 'north dakota', lwd=1, col="grey60")
  maps::map('state', add=TRUE, lwd=2)
  points(subset(nd.features.d, name=="Fargo")[4:5],
          pch=21, bg="blue", col="white", cex=3)
  
  # Add more points:
  with(subset(nd.features.d, feature=="city"),
       points(lon, lat, pch=21, bg="blue", col="white", cex=2) )
  with(subset(nd.features.d, feature=="city"), 
       text(lon, lat, pos=3, labels=name, col="blue", cex=1) )
  with(subset(nd.features.d, feature=="rec"), 
       points(lon, lat, pch=24, bg="orange", col="white", cex=2))
  with(subset(nd.features.d, feature=="rec"), 
       text(lon, lat, pos=3, labels=name, col="black", cex=1))
  
# Better mapping with ggplot
  # Get data
    states.md <- map_data("state")
    l48 <- subset(states.md, region !="alaska")
  
  # Plot map with ggplot
    # note use of geom_polygon
    (L48.gg <- ggplot() +
          geom_polygon(data=l48, aes(x=long, y=lat, group=group), 
                       color="white", fill="grey80", size=0.25) )
    L48.gg + theme_bw() 
    L48.gg + theme_nothing() # from ggmap
    L48.gg + theme_map()     # from rangeR
    
    (L48.gg1 <- L48.gg  + theme_bw() + 
                  coord_map() ) # Manage projections with a coord_
    L48.gg1 + coord_map("stereographic")
    L48.gg1 + coord_map("polyconic")
    L48.gg1 + coord_map("conic", 30)
    L48.gg1 + coord_map("conic", 90)
    L48.gg1 + coord_map("guyou")
    
    (L48.gg <- L48.gg  + theme_map() + 
                coord_map("polyconic")) 

# Add spatial point data via geom_point:
  L48.gg +  geom_point(data=nd.features.d, 
                       aes(x=lon, y=lat, 
                           shape=feature, color=feature))
# Import spatial points data 
  class(fear.us)
  # Extract, combine relevant data for mapping
  fear.us@data$id <- rownames(fear.us@data)
  fear.us.pts <- data.frame(objectid=fear.us$OBJECTID, 
                            long=fear.us$coords.x1, 
                            lat=fear.us$coords.x2)
  
  # View extent of species distribution:
  L48.gg + geom_point(data=fear.us.pts, aes(x=long, y=lat)) 
  
  # Crop map to regional area of interest:
  L48.gg + geom_point(data=fear.us.pts, 
                      aes(x=long, y=lat)) +
          coord_fixed(ratio = 1.3, 
                      xlim=c(-104, max(l48$long)), 
                      ylim=l48$lat)
  
# ggplot a shapefile
  class(us.epa.III)
  # First, make sure Coordinate Reference Systems match up 
  proj4string(fear.us)  # Long Lat WGS84
  proj4string(us.epa.III) # Aaea = Albers Equal Area

  # CRS differ 
  # spTransform converts CRS of any Spatial* to long/lat
  epa.regions <- us.epa.III
  epa.regions  <- spTransform(us.epa.III, CRS("+init=epsg:4326"))
  epa.regions@data$id <- rownames(epa.regions@data)
  epa.regions.points <- broom::tidy(epa.regions, region="id")
  epa.regions.df <- plyr::join(epa.regions.points, epa.regions@data, by="id")
  
  # Map species distribution on top of ecoregions
  L48.gg + geom_polygon(data=epa.regions.df[epa.regions.df$NA_L1CODE %in% 
                                              c('8','9'),], 
                        aes(x=long, y=lat, group=group, 
                            fill=NA_L1NAME), alpha=0.5) +
    geom_point(data=fear.us.pts, aes(x=long, y=lat)) +
    coord_fixed(1.3, xlim=c(-104, max(l48$long)), ylim=l48$lat)
  
#
# Package sf is a 'tidy" solution for handling spatial data
#
  # use st_read to access your original shapefile 
  # st_as_sf works on Spatial* objects already in R
  
  st_as_sf(us.epa.III ) %>%
  ggplot( ) + theme_bw() + 
    geom_sf(aes(fill = NA_L1NAME), alpha=0.5) +
              scale_fill_viridis("Level 1 ecoregion", 
                                 discrete=TRUE, direction =-1)
  
  # Convert data to sf objects
  epa_sf <- st_as_sf(us.epa.III )
  fear_sf <- st_as_sf(fear.us ) %>%
                st_crop(xmin=-104, xmax=-67, 
                        ymin=25.13, ymax=49.38 ) 

  ggplot() + theme_nothing() + 
    geom_sf(data=filter(epa_sf, NA_L1CODE %in%  c('8','9')), 
            aes(fill=NA_L1NAME), alpha=0.5 ) + 
    geom_sf(data=fear_sf, pch=21, bg="white", 
            col="black", stroke=1.5) + 
    scale_fill_viridis("Level 1 ecoregion", 
                       discrete=TRUE, direction =-1)
