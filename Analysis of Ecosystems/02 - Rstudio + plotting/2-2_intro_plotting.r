# An introduction to R programming
# NDSU Analysis of Ecosystems
# Devan Allen McGranahan (devan.mcgranahan@ndsu.edu)

# Lesson 3: Introduction to graphing in R.
  # Basic plotting with plot() + an introduction to ggplot

##
## S T A R T  H E R E

# Install and load packages used in this session 

  install.packages("ggplot2","plyr","ggthemes")
  library(plyr)

# You will need our customized mtcars2 file.
    # If you had problems loading it on Tuesday, 
    # you should re-load the file from Google Drive 
    # to ensure you have all the correct columns. 
  
    # ~Remember!~ Different functions for different file types:
    #   use read.csv() for .csv files
    #   use load() for .Rdata files 
      
      # To import mtcars2.csv:
        # Step 1: Save mtcars.csv to your local folder (E:/R/data)
      
        # Step 2: Set your working directory: 
          setwd("E:/R/")
      
        # Step 3: Load file: 
          mtcars2 <- read.csv(file="./data/mtcars2.csv")
          # or
          mtcars2 <- read.csv(file.choose())

      # To load an existing R object called mtcars2:
         
          # Step 1: Set your working directory: 
          setwd("E:/R/")
          
          # Step 2: Load R object: 
          load(file="./data/mtcars2.Rdata")
          # or
          load(file.choose())

##
## B A S I C  P L O T T I N G  W/ plot()  F U N C T I O N S 
##

# Do your data have categorical predictors, continuous response?
# Box plots are a great option. 

class(mtcars2$hp)
class(mtcars2$cyl) 

# boxplot() takes arguments in the formula format (y ~ x, data)

# Default settings don't look bad for simple plots:
boxplot(hp ~ origin, mtcars2)
boxplot(hp ~ cyl, mtcars2)

# But even just adding one variable gets messy: 

boxplot(hp ~ cyl + origin, mtcars2)
x11() # You can always use this to pop up a new graphics window 
      # Pretty old-school; rarely will you do this with Rstudio?
boxplot(hp ~ cyl + origin, mtcars2, xaxt="n", 
        xlab="Number of cylinders", ylab="Horsespower", 
        cex.lab=1.3, cex.axis=1.3, las=1, lwd=2, col=c("blue","orange"))
axis(side=1, at=c(1.5,3.5, 5.5), labels=c("4", "6", "8"), cex.lab=1.3)
legend("topleft", title="Origin", c("Foreign","Domestic"), 
       fill=c("blue", "orange"), cex=1.2, bty="n")

# xlab and ylab assign axis labels
# cex.axis and cex.lab change font size of axis and axis labels
# lwd changes the width (weight) of lines
# las=1 makes Y axis labels read parallel 
# ?par gives you *tons* more

# Add means to the boxplot: 
hp.box <- boxplot(hp ~ cyl + origin, mtcars2)
points(seq(hp.box$n), agg.example$hp.mean, pch=24, col="white", bg="blue", cex=3)
agg.example 

agg.example2 <- rbind(data.frame(origin="domestic", cyl="4",
                                 hp.mean=0, hp.sd=0), agg.example) 
points(seq(hp.box$n), agg.example2$hp.mean, pch=24, col="white", bg="grey30", cex=2)
  
# Scatterplots are often used when both variables are continuous: 
  
  class(mtcars2$hp)
  class(mtcars2$mpg)
  
# plot() function takes formula format: 
  
  plot(mpg ~ hp, mtcars2)
 
  # Think carefully when setting up your formula: 
  # Which variable is independent? (it goes in for X)
  # Which variable depends on the other? (it goes in for Y)
  
  x11(width=8, height=4) ; split.screen(c(1,2))
  screen(1) ; plot(mpg ~ hp, mtcars2, las=1)
  screen(2) ; plot(hp ~ mpg, mtcars2, las=1)
  close.screen(all=TRUE) 
  
  dev.off() # Kill pop-up graphics device, return to Rstudio
  
# Fitting trend lines 
  
  # Linear relationships beg to have a line drawn through them. 
  # R includes functions to automatically fit and plot lines. 
  # e.g., abline() and lm() are seperate functions, but abline()
  # "knows" how to read lm() (linear model) results for line info:
  
  abline(lm(mpg ~ hp, mtcars2))
  
  # Different pars can be added - to abline - to customize:
  
  abline(lm(mpg ~ hp, mtcars2), lwd=2)
  abline(lm(mpg ~ hp, mtcars2), lwd=5, lty=2)
  
  # Note the overplotting. 
  # To get a new line, must start from scratch:
  
  plot(mpg ~ hp, mtcars2, las=1)
  abline(lm(mpg ~ hp, mtcars2), lwd=2, lty=2)

# Combining continuous and categorical variables
  
  # Perhaps cars with different origins have unique 
    # mpg ~ hp relationships?
    # Adventures with subset(): 
  
  plot(mpg ~ hp, mtcars2, las=1, type="n") # Make a blank plot
    points(mpg ~ hp, subset(mtcars2,origin=="foreign"),pch=1)
    points(mpg ~ hp, subset(mtcars2,origin=="domestic"),pch=19)
    
    abline(lm(mpg~hp, subset(mtcars2,origin=="foreign")),lty=1)
    abline(lm(mpg~hp, subset(mtcars2,origin=="domestic")),lty=2)

  # Customize the graph: 
    # Improve aesthetics, provide more info about the data: 
    
    plot(mpg ~ hp, mtcars2, las=1, type="n", 
         cex.axis=1.4, cex.lab=1.4,
         xlab="Engine power (hp)", xlim=c(50,350), 
         ylab="Fuel economy (mpg)", ylim=c(10,35))
    
    points(mpg ~ hp, subset(mtcars2, origin=="foreign"), 
           pch=19, cex=1.4, col="orange")
    points(mpg ~ hp, subset(mtcars2, origin=="domestic"), 
           pch=17, cex=1.4, col="blue")
    
    abline(lm(mpg~hp, subset(mtcars2, origin=="foreign")), 
           lwd=2, col="orange", lty=1)
    abline(lm(mpg~hp, subset(mtcars2, origin=="domestic")), 
           lwd=2, col="blue", lty=2)
    
    legend("topright", title="Car origin", c("Foreign","USA"), 
           lty = c(1,2), col=c("orange","blue"), 
           pch=c(19, 17), cex=1.4, lwd=2, bty="n" )
    
##
## I N T R O D U C I N G  ggplot 
##
    library(ggplot2)
    
    # Two main functions: 
      # qplot() or "quick plot" - takes aesthetics and geometry in-line
      # ggplot() builds plot as aesthetics and geometries are added
        # Some aesthetics (aes) include: x, y, color, shape, line type...
        # Some geometries (geoms) include: point, boxplot, line, text...
    
    # Neither qplot nor ggplot read formula format. 
    # Arguments must be specified. 
    # qplot assumes order is x, y:
      qplot(cyl, mpg, data = mtcars2)  # Incorrect geometry assumption
      qplot(cyl, mpg, data = mtcars2, geom="boxplot")
      qplot(hp, mpg, data = mtcars2)  # Correct geometry assumption
     
    # The power of ggplot lies in *aesthetics mapping*
    # Instead of subsetting and specifying colors, sizes, etc, 
    # You tell ggplot which variables you want to plot by, and ggplot
    # finds them in your data and applies default (but customizeable)
    # parameters like color (often "colour" since the author is Kiwi):
    
      qplot(cyl, mpg, data = mtcars2, colour=origin, geom="boxplot")
      qplot(hp, mpg, data = mtcars2, colour=origin)
      qplot(hp, mpg, data = mtcars2, colour=origin, shape=am)
      qplot(hp, mpg, data = mtcars2, facets=~origin, colour=am)
      qplot(hp, mpg, data = mtcars2, facets=am~origin, colour=cyl)
    
    # Adding regression lines are beyond qplot. Need to jump to ggplot:
    
      ggplot(data=mtcars2, aes(x=hp, y=mpg)) # Empty! No geometry
      ggplot(data=mtcars2, aes(x=hp, y=mpg)) +
        geom_point() 

    # ggplot can be object-oriented, helps build plots:
      ggp <- ggplot(data=mtcars2, aes(x=hp, y=mpg)) +
                geom_point() 
      ggp
      (ggp <- ggp + stat_smooth(method="lm", se=FALSE) ) 
    
      ggp + facet_wrap(~am)
      ggp + facet_grid(origin~am)
    
    # Themes
    
      # Several defaults:
        ggp + theme_bw() # No background, major + minor gridlines
        ggp + theme_linedraw() # No background, no minor gridlines
    
      # Many options developed:
        library(ggthemes)
        ggp + theme_wsj()   # Print like a Wall Street Journal graph
        ggp + theme_economist()  # Or one from the Economist
        ggp + theme_tufte() 
        # Themes based on retro stat software:
          ggp + theme_stata() 
          ggp + theme_excel() 
          ggp + theme_base() # Recognize this one??
  