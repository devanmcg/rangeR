# An introduction to R programming
# NDSU Analysis of Ecosystems
# Devan Allen McGranahan (devan.mcgranahan@ndsu.edu)

# Lesson 3.1: Introduction to graphing in R, continued

# Install and load packages for use in this session 

  install.packages(c("tidyverse","plyr","ggthemes","devtools"))
  devtools::install_github("dill/emoGG")

library(ggplot2) # in the tidyverse
    
##
##  Tour of geoms 
##
    # Using an example ggplot dataset
    mpg  # it is a tibble, don't need str() 
    
# Discrete independent variables 
    # One variable: bar graphs with geom_bar
    
    bg <- ggplot(mpg, aes(factor(cyl)))
    bg + geom_bar() # Automatic count per group on y axis
    
    
    # Two variables (continuous response variable)
      
      bp <- ggplot(mpg, aes(x=factor(class), y=hwy))
      bp + geom_boxplot() 
      bp + geom_violin() # Insight into data distribution
      bp + geom_violin() + geom_jitter() 
      bp +  geom_violin() + 
            geom_jitter(width=0.5, alpha=0.3) 
      
      bp +  geom_boxplot() + 
            geom_jitter(width=0.5, alpha=0.3) 
      
      bp +  geom_boxplot(outlier.colour=NA) + 
            geom_jitter(width=0.5, alpha=0.3) 
      
      bp +  geom_boxplot(outlier.colour=NA) + 
            geom_jitter(aes(colour=factor(cyl)), width = 0.25) 
      
    # A bit of customization 
      
      # More intuitive order of x axis with reorder function. 
      # arguments go variable, data, function to apply in sorting
      bp <- ggplot(mpg, aes(x=reorder(class, hwy, mean), y=hwy))
      bp + geom_boxplot() 
      
      # Not typically how plots work; usually go large -> small.
      # Reverse the order by changing the sign of the variable: 
      
      bp <- ggplot(mpg, aes(x=reorder(class, -hwy, mean), y=hwy))
      bp <- bp + geom_boxplot() 
      bp
      
      # Default axis labels weren't great before, and they've
      # gotten uglier. Let's specify them:
      
      bp + labs(x="Vehicle class", 
                y="Highway fuel economy (mpg)" )
      
      # Two variables: continuous vs. continuous 
      
      sp <- ggplot(mpg, aes(x=cty, y=hwy))
      sp + geom_point() # Boring!
      
      library(emoGG)
      sp + geom_emoji(emoji="1f697") 
      
      emoji_search("cats")
      
# Can also "pipe" in to ggplot with dplyr:

  library(dplyr) # in the tidyverse
  
  mpg %>%
    mutate(class = reorder(class, -hwy, mean)) %>%
      ggplot() + geom_violin(aes(x=class, y=hwy)) 

# Piping makes it easy to calculate summary stats for plotting:
  
  library(forcats) # in the tidyverse, for fct_reorder() 

mpg %>%
    group_by(class) %>%
      summarise(Mean = mean(hwy), 
                se   = sd(hwy) / sqrt(length(hwy))) %>%
        mutate(class = fct_reorder(class, -Mean)) %>%
    ggplot(aes(x=class)) + # an aes in the ggplot call is universal (all geoms)
      geom_errorbar(aes(ymin = Mean-se, # an aes in a geom is geom-specific
                        ymax = Mean+se), 
                    size=1.25, width=0.2) +
      geom_point(aes(y=Mean), 
                 shape=21, size=4, fill="black",
                 color="white", stroke=1.5) +
      labs(x = "Vehicle class", 
           y = "Highway mileage (mean +/- s.e.)")
  