# Install, load packages

install.packages("pacman")
pacman::p_load(ezknitr, knitr, googlesheets, maps, rgdal, ggmap, plyr, 
                   maptools, tidyverse, grid, broom, 
                   tm, SnowballC, wordcloud, gridExtra)

setwd("D:/R") 
ezspin(file="./script/survey analysis2.R", 
out_dir = "output", fig_dir = "figures", keep_md = FALSE)