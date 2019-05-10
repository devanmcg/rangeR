

devtools::install_github("ropensci/geonames")

options(geonamesUsername="devanmcg")

library(geonames)

names((fargo_df <- GNsearch(name_equals = "Fargo", country = "US", adminCode1="ND")))
fargo_coords <- fargo_df[1, c("lng", "lat")] %>%
                  plyr::rename(c("lng"="lon")) %>%
                  mutate(feature="city", 
                         name="Fargo")

nd.features <- data.frame(feature=c("city", "city", "city", "city", 
                                    "rec", "rec", "rec", "rec", 
                                    "rec", "rec"), 
                          name=c("Fargo", "Grand Forks", "Bismarck", "Minot", 
                                 "Hettinger", "Williston", "Carrington","Dickinson",
                                 "Streeter", "Langdon") )
nd.features.d <- 
  nd.features %>%
    split(.$name) %>%
      map( ~ GNsearch(name_equals = .x$name, country = "US", 
                      adminCode1="ND", featureClass="P")) %>%
        map_dfr(~ (.))  %>%
    full_join(nd.features) %>%
    mutate(state = adminName1, 
           lon=as.numeric(lng), 
           lat = as.numeric(lat)) %>%
    select(feature, name, state, lon, lat) 
    
save(nd.features.d, file="./example datasets/nd.features.d.Rdata")

