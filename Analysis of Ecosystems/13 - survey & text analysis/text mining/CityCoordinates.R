options(geonamesUsername="devanmcg")
pacman::p_load(geonames)


cities <- data.frame(city=c("Bismarck", "Fargo", "Minneapolis", "Kansas City",
                            "Des Moines", "New York City",
                            "San Francisco", "Houston"), 
                     state=c("North Dakota", "North Dakota", "Minnesota","Missouri", 
                             "Iowa", "New York", "California", "Texas") )

    twitter_coords <- data.frame()            
for (i in 1:length(cities$city)) {
  require(dplyr)
  c = cities %>% slice(i)  %>% mutate(state = state.abb[match(state,state.name)])
  cd <- GNsearch(name_equals = as.character(c$city), 
                 adminCode1=as.character(c$state) ) 
  cd <- cd %>% select(toponymName, adminCode1, lat, lng) 
  colnames(cd) <- c("city", "state", "lat", "lon")
  twitter_coords <- rbind(twitter_coords, cd)
  }            

    save(twitter_coords, file="./example datasets/twitter_coords.Rdata")
    