# Install, load packages

install.packages("pacman")
pacman::p_load(ezknitr, knitr, maps, rgdal, ggmap, plyr, 
                   maptools, tidyverse, broom, grid, gridExtra,
                   tm, SnowballC, wordcloud)

setwd("E:/R") 
ezspin(file="./script/SurveyAnalysis.R", 
out_dir = "output", fig_dir = "figures", keep_md = FALSE)
