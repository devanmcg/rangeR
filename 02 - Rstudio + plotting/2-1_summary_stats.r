# An introduction to R programming
# NDSU Analysis of Ecosystems
# Devan Allen McGranahan (devan.mcgranahan@ndsu.edu)

# Lesson 2: Introduction to R, continued
# More functions for managing data + basic plotting with plot() 

##
## S T A R T  H E R E

# Install and load packages used in this session 

install.packages("pacman")
library(pacman)

# You will need the mtcars2 file we made last time.
# Hopefully you saved it; use these lines to load it.
# (If you didn't save it, go back to script file 
# "1-1_intro-R_basic-functions.r" and re-do it.)

# Set working directory: 
setwd("E:/R/")

# Load file: 
load("./data/mtcars2.Rdata")
# or
load(file.choose())

# Calculate descriptive statistics

# function aggregate() uses standard R formula format (y ~ x, data) 
# to define relationships between variables one wants to calculate. 
# This function is analogous to making a PivotTable in Excel.

aggregate(hp ~ cyl, data=mtcars, FUN=mean)  
options(digits=3) # Set sig. figs. for session 
aggregate(hp ~ origin + cyl, data=mtcars2, FUN=mean)  
aggregate(cbind(mpg, hp) ~ origin + cyl, data=mtcars2, FUN=mean) 

# function ddply in the plyr package is another option. 
# it is preferred because it is vectorized but note: 
# plyr functions use the tidyverse grammar: 

pacman::p_load(plyr)

ddply(mtcars2, .(origin, cyl), summarize, mean=mean(hp)) 

# ddply also has an advantage because it can do more at once: 

# Mean and SD takes three steps with aggregate()...
(agg.example <- aggregate(hp ~ cyl + origin, data=mtcars2, FUN=mean) ) 
colnames(agg.example)[3] <- "hp.mean"
agg.example[,"hp.sd"] <- aggregate(hp ~ cyl + origin, data=mtcars2, FUN=sd)$hp 
agg.example

# ... but ddply can combine the two: 
ddply(mtcars2, .(cyl, origin), summarize, hp.mean=mean(hp), hp.sd=sd(hp)) 

# In some contexts the dplyr route is preferable: 

pacman::p_load(dplyr)

mtcars2 %>%
  group_by(origin, cyl) %>%
  summarise(hp.mean = mean(hp), 
            hp.sd = sd(hp))
