install.packages(c("devtools","highcharter",
                   "jsonlite", "tidyr","viridis"))
devtools::install_github("jbkunst/highcharter")
library(highcharter)
library(jsonlite)
library(tidyr)
library(viridis)

URL <- "http://graphics8.nytimes.com/newsgraphics/2016/01/15/drug-deaths/c23ba79c9c9599a103a8d60e2329be1a9b7d6994/data.json"

  data("uscountygeojson")

data <-  fromJSON(URL) %>% 
  tbl_df() %>% 
  gather(year, value, -fips) %>% 
  mutate(year = sub("^y", "", year),
         value = ifelse(is.na(value), 0, value))

ds <- data %>% 
  group_by(fips) %>% 
  do(item = list(
    fips = first(.$fips),
    sequence = .$value,
    value = first(.$value))) %>% 
  .$item

# Define color ramp 
  n <- 6
  stops <- data.frame(q = 0:n/n,
                      c = substring(viridis(n + 1), 0, 7),
                      stringsAsFactors = FALSE)
  stops <- list_parse2(stops)

# Highchart

hc <- highchart(type = "map") %>% 
  hc_add_series(data = ds,
                name = "drug deaths per 100,000",
                mapData = uscountygeojson,
                joinBy = "fips",
                borderWidth = 0.01) %>% 
  #hc_colorAxis( minColor = "#FFFFFF", maxColor = "#434348") %>% 
  hc_colorAxis( stops = stops) %>%  
  hc_title(text = "Ripple of Drug Overdose Deaths, 2002-2014") %>% 
  hc_legend(layout = "vertical", reversed = TRUE,
            floating = TRUE, align = "right") %>% 
  hc_add_theme(hc_theme_smpl()) %>%
  hc_motion(  enabled = TRUE,
              axisLabel = "year",
              labels = sort(unique(data$year)),
              series = 0,
              updateIterval = 50,
              magnet = list(
                round = "floor",
                step = 0.1     )  )

hc



