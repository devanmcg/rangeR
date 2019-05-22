# An introduction to R programming!
# NDSU Analysis of Ecosystems
# Devan Allen McGranahan (devan.mcgranahan@ndsu.edu)

# Lesson 1: Introduction to R
  # Basic functions for loading, evaluating, and managing data 

# Remember ?functionname and ??functionname for getting help!

## R as a calculator: 
	2+2 # R should return 4 
	2*2 # R will return 4
	2*2+6 # R will return 10
	6+2*2 # R will return 10, it knows the order of operations
	6+(2*2) # R also recognizes parentheses 

	# R is "object-oriented" - define an object and R will remember it:
	answer <- 2+(20*2)
	answer
	answer*2
	new.answer <- answer*2
	new.answer
	 
	# Introducing functions 
	# Researchers often want to find the mean of data: 
	
	(24 + 13 + 12 + 22 + 15) / 5 
	
	data <- c(24, 13, 12, 22, 15)
	data
	
	data / 5 
	sum(data) / 5 
	
	length(data)
	sum(data) / length(data)
	mean(data)
	
	Meaner <- function(x) { sum(x) / length(x) }
	
	Meaner(data)
	
	Meaner <- function(x) { 
	  m <- sum(x) / length(x)
	  m1 <- noquote(paste(m, "!", sep="") )
	  return(m1)
	}
	Meaner(data)
	
## Some housekeeping

# Setting working directories
	# Working directories help pull data from, and save to, specific files. 
	# You can always see what your current working directory is:
		getwd() # print the current working directory (cwd)
		ls()    # list the objects in the current workspace
		
	# You can change the working directory: 
		setwd("E:/R/")  # ~!~ note: use / or \\ instead of \ in windows ~!~
	
	# Saving things 
		# Save specific objects. 
		# Always a good idea: after importing and manipulating data, save specific objects
		save(file.name, file="./file.name.Rdata") # saves to cwd
		load("./file.name.Rdata") # Bring R objects into the workspace from cwd. 
		
		# load() is only for .Rdata objects. 
		# Many other functions import (read) file types into R. 
		# .csv is a common file type to go from a spreadsheet to R: 
		
		mtcars.origins <- read.csv(file="./data/mtcars_origins.csv")
		
		# or if you don't know the path (but you should know the path!):
		
		mtcars.origins <- read.csv(file=file.choose()) 
		
		# R studio also has a clicky import feature but I strongly recommend hard-coding 
		# to your data files with the appropriate read.xx() function. 
			
		# Save workspace: at the end of your session, you can save everything on the current workspace.
		# But there are parties who believe strongly against this practice: 
		# https://www.r-bloggers.com/using-r-dont-save-your-workspace/
		save.image()
	
	# Types of R objects
	 	# Libraries/packages are how the R community shares extended functionality 
		# Hundreds are available "officially" through r-cran (the preferred way to access and reference)
		# Many more functions are available on github and personal and academic websites
		install.packages("permute") # prompts a download procedure. You need to select a source.
		library(permute) # Loads the library/package. Necessary each time R is restarted 
		                # & included functions desired.
		data(jackal) # Calls up some data available in the permute package.
		
# Some diagnostic functions
		
		# Evaluate an entire R object 
		
		dim(cars)  	  # Dimensions of the object
		names(cars)	  # Displays column headings
		head(mtcars)	# Displays top six lines of the data
		tail(cars)    # Displays bottom six lines
		class(cars)	  # Data frame, matrix, etc.

	 # Evaluate specific columns
		
		head(cars) # Top six rows of both columns
		head(cars$speed) # Top six rows of column "speed"
		head(cars[1]) # Top six rows of column "speed"
		
	  # Evaluate specific rows and cells 
		
		cars[1,] # Display row 1
		cars[1:3,] # Display rows 1 through 3
		cars[3:5,] # Display rows 3 through 5
		cars[5,2] # Display component located in row 5, column 2
		
# Classes within objects 
		
		class(cars$speed) # Returns class of specific column
		str(cars)         # Provides all evaluation info
		                  # of object and components
		
		str(mtcars)       # A more complex dataset?
		
		# Changing component classes with as. functions
		
		mtcars$cyl <- as.factor(mtcars$cyl)
		mtcars$vs <- as.factor(mtcars$vs)
		mtcars$gear <- as.factor(mtcars$gear)
		mtcars$carb <- as.factor(mtcars$carb)
		str(mtcars)
		
		# Wait, what the heck is "am" anyway? Go to help:
		?mtcars
		
		# Change column data to something more useful
		pacman::p_load(plyr)  # Note :: pulls a function (rhs) 
		                      # from a package (lhs) without loading 
		                      # package. Can help avoid conflicts.
		
		mtcars$am <- revalue(as.factor(mtcars$am), c("0"="Automatic", 
		                                             "1"="Manual"))
		
	# Adding variables 
		
		# First, the mtcars dataset is a tad wonky. 
		# Notice how the makes and models for the cars isn't actually
		# a variable column (info isn't in str(mtcars)), but is rather
		# assigned to the names of the rows:
		
		rownames(mtcars)
		
		# We can assign row names to a new column called make.model:
		
		mtcars$make.model <- factor(rownames(mtcars) ) 
		
		# This gives us a reference point to automatically look up 
		# a variable from one data.frame in another data.frame
		# using the merge function (similar to HLOOKUP in Excel)
		
		mtcars2 <- merge(x=mtcars.origins, y=mtcars, 
		                 by.x="make.model", by.y="make.model")
		
		# Add a variable using a logical string.
		# ifelse reads as "if X, then Y; otherwise Z (because not X)"
		
		mtcars2[,"origin"] <- with(mtcars2, ifelse(country=="USA", 
		                                            "domestic", "foreign"))
		head(mtcars2)
		                                               
		# although ifelse first appears either/or, ifelse statements
		# can be nested to add a series of options: 
		
		mtcars2[,"continent"] <- with(mtcars2, ifelse((country=="USA"), 
		                                           "North America", 
		                                           ifelse((country=="Japan"), 
		                                                  "Asia", "Europe")))
		head(mtcars2)

#	
#   T I D Y V E R S E 
#
		# The tidyverse offers a totally different way to do this: 
		pacman::p_load(tidyverse)
		
		mtcars2 <- mtcars2 %>% mutate(continent = case_when(
              		                  country == "USA" ~ "North America", 
              		                  country == "Japan" ~ "Asia", 
              		                  TRUE ~ "Europe" ))
		head(mtcars2)
		
		# Note the pipe operator: %>%
		# It pours R objects along through a chain of functions 
		# in one run with no intermediate objects. 
		# The advantage is that many manipulations can be made at once
		# using the tidyverse "grammar" and its many functions ("verbs"):
		
		mtcars.origins %>% 
		  full_join(mtcars, by = "make.model") %>% 
		    mutate(origin = ifelse(country=="USA", "domestic", "foreign"), 
		           continent = case_when(
    		             country == "USA" ~ "North America", 
    		             country == "Japan" ~ "Asia", 
    		             TRUE ~ "Europe" )) %>%
          arrange(make.model) -> mtcars3 # ooh look, he named the object
    		                                 # at the **end** of the pipe!
		
		str(mtcars3) # tibble object
		str(mtcars2) # regular old data.frame
		mtcars3 # tibble objects automatically give a tidy preview
		
		
		# After all that work - make sure to save!
		
		getwd()
		save(mtcars2, file="./data/mtcars2.Rdata") 
		# or
		save(mtcars2, file=file.choose()) 
		# or 
		write.csv(mtcars2, file="./data/mtcars2.csv")
