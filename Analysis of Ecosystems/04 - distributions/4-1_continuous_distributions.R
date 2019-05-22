setwd("E:/R/")
if (!require("pacman")) install.packages("pacman")
pacman::p_load(plyr, tidyverse)


  load(file="./data/swha.dat.Rdata")
  
# The normal (Gaussian) distribution 

  plot(function(x) dnorm(x), -3, 3, las=1, ylab = "Density", xlab = "Value",
                                           main = "Normal distribution")
  
# We can use rxx functions to sample from distributions
  # Illustrates the role of sample size:
  
  norm10 <- rnorm(n = 10, mean = 0, sd = 1)
  hist(norm10, las = 1, main = "n = 10", col = "lightgreen", breaks = seq(-3,3,1))
  
  norm50 <- rnorm(n = 50, mean = 0, sd = 1)
  hist(norm50, las = 1, main = "n = 50", col = "lightgreen", breaks = seq(-3,3,1))
  
#  
# Let's compare a few sample sizes on some randomly-generated data
#
  # First set some parameters for data generation
    sample.sizes <- c(10, 100, 1000, 100000)  # 4 same sizes to compare
  
  # Distribution functions parameterized with first two data moments:
    # mean (mu) and standard deviation (sigma)
    norm.parms <- data.frame(mu = 0, sigma = 1 )
  
  # Get some fake actual data
    sample.data <- data.frame( )
    str(sample.data) # Empty
    
  # Use function rnorm() to get random draws from normal distribution
    for(i in 1:length(sample.sizes)) {
      V <- rnorm(n=sample.sizes[i], mean = norm.parms$mu, sd = norm.parms$sigma)
      N = paste("n =", length(V), sep=" ")
      d <- data.frame(n=N, value=V)
      sample.data <- rbind(sample.data, d) 
      }
    
    str(sample.data) 
  
  # Basic histograms of actual data
    dist.gg <- 
      ggplot(sample.data, aes(x=value)) + theme_bw() + 
        geom_histogram(aes(y=..density..),      
                       binwidth=0.1,
                       colour="black", fill="lightgreen") +
        facet_wrap(~n, ncol = 2, scales = "free_y")
  
  # Show density of actual data
    (dist.gg <- dist.gg + geom_density(alpha=.2, fill="#FF6666") )
    
# Find the theoretical distributions of the fake actual data
    
    sample.dist <- data.frame()
    
    # Use function dnorm() to get theoretical densities

    for(i in 1:length(sample.sizes)) {
      dat <- filter(sample.data, n == unique(n)[i]) 
      x <- seq(-3, 3, length=100) 
      d1 <- data.frame(X = x, Y = dnorm(x, mean=norm.parms$mu, sd=norm.parms$sigma)) 
      N = as.character(unique(dat$n))
      d2 <- data.frame(n=N, d1)
      sample.dist <- rbind(sample.dist, d2) 
        }

# Compare theoretical distribution to actual distribution
    # using geom_line to support facetting
  dist.gg + geom_line(data=sample.dist, aes(x=X, y=Y), 
                      color="blue", size=1.1)
  
#
# Now let's work with some real actual data 
#

# View data on normal scale 
  
  hp.gg <- 
    mtcars %>%
    ggplot(aes(x=hp)) + theme_bw() + 
    coord_cartesian(xlim=c(0,350)) + 
    geom_histogram(aes(y=..density..),      
                   binwidth=10,
                   colour="black", fill="lightgreen") +
    geom_density(alpha=.2, fill="#FF6666") 
  hp.gg
  
  # Get the theoretical distribution density 
    # First need parameters mu and sigma 
  
  summarize(mtcars, mu = mean(hp), sigma = sd(hp) )
                     
  # New ggplot component, stat_xx; adds statistics to graph. 
  # stat_function allows a function (here, dnorm) to chug on the data 
  # arguments the function needs are passed as a list via args=
  
  hp.gg + stat_function(data=mtcars, 
                        fun = dnorm, 
                        args=list(mean=146.7,    # pass mu to dnorm
                                    sd=68.6),    # pass sigma to dnorm
                        colour="blue", size=1.1) 
  

# View data on log scale 
  hp.log <- 
    ggplot(mtcars, aes(x=log(hp))) + theme_bw() + 
    geom_histogram(aes(y=..density..),      
                   binwidth=.1,
                   colour="black", fill="lightgreen") +
    geom_density(alpha=.2, fill="#FF6666") 
  hp.log 
  
# Find summary stats for the log-transformed data
  mtcars %>%
    mutate(hp = log(hp)) %>%
      summarise(mu = mean(hp), sigma = sd(hp))
  
  hp.log + stat_function(data=mtcars, 
                         fun = dnorm, 
                         args=list(mean=4.9, 
                                  sd=0.48), 
                         colour="blue", size=1.1) 
  
# Instead of a transformation, fit a different continuous function 
  ?dgamma
  
  # Uh oh, arguments are shape and rate, not mean and 
  # Alternative way to get distribution parameters:
  # Numerically-optimization via maximum-likelihood
    MASS::fitdistr(mtcars$hp, "normal") 
    MASS::fitdistr(mtcars$hp, "lognormal") 
    MASS::fitdistr(mtcars$hp, "Gamma") 
  
  # Fit a Gamma curve over the non-transformed data
    hp.gg + stat_function(data=mtcars, 
                          fun = dgamma, 
                          args=list(shape=4.88,
                                    rate=0.03),   
                          colour="blue", size=1.1) 
    
# Try this out on some other data 
# Question: Are hawks attracted to Rx fires? 
# Method: Count hawks before and during burns
  
  str(swha.dat) 
  
  gg1 <-  
    swha.dat %>%
      gather(key=period, value=count,  -burn) %>%
    ggplot(aes(x=period, y=count)) + theme_grey(16)  
  
  (gg2 <- gg1 + geom_violin(size=1.25, color="blue") ) 
  
  gg2 + geom_point(shape=1) +
        geom_line(aes(group=burn))
  
  swha.dat <- swha.dat %>% mutate(diff = during - before)
  
  swha.gg <- ggplot(swha.dat, aes(x=diff)) + theme_bw(16) + 
                      geom_histogram(aes(y=..density..),      
                                     binwidth=1,
                                     colour="black", fill="lightgreen") +
                      geom_density(alpha=.2, fill="#FF6666") 
  swha.gg
  
  swha.gg + stat_function(data=swha.dat, fun = dnorm, 
                          args=list(mean=mean(swha.dat$diff),
                                    sd=sd(swha.dat$diff)),
                          colour="blue", size=1.1) 

# Fit a gamma distribution 

  MASS::fitdistr(swha.dat$diff+1, "Gamma") # Add 1 because Gamma needs > 0
  
  swha.gg + stat_function(data=swha.dat, fun = dgamma, 
                          args=list(shape=1.72, 
                                    rate=0.09), 
                          colour="blue", size=1.1) 

# What difference does it make?
  
  swha.r <- data.frame(Gaussian = rnorm(1000, mean=mean(swha.dat$diff),
                                              sd=sd(swha.dat$diff)), 
                       Gamma = rgamma(1000,shape=1.72, 
                                           rate=0.09 )-1 ) # Subtract the 1 added above!
                                                            
  swha.CIs <- swha.r %>% 
                gather(key=distribution) %>%
                  group_by(distribution) %>%
                    summarize(lower = quantile(value, prob=0.025,na.rm=TRUE),
                              actual = mean(swha.dat$diff),
                              modeled = mean(value),
                              upper = quantile(value, prob=0.975,na.rm=TRUE) ) 
  CI.gg <- 
    swha.CIs %>%
      ggplot(aes(x=distribution)) + theme_bw(16) + 
        geom_hline(yintercept = 0, color="darkgrey") +
        geom_hline(aes(yintercept=actual), linetype = 2) + 
        geom_errorbar(aes(ymin = lower, ymax = upper), 
                      size=1.5, width=0.2) + 
        geom_point(aes(y=modeled), shape=21, size=6,
                      fill="black", color="white", stroke=2) +
      labs(y="Attraction effect of Rx fire on SWHA") +
      geom_text(x=1.5, y=18, label = "Actual\nmean")
  CI.gg

# Check out how actual data fit within confidence intervals
  CI.gg + geom_rug(data=swha.dat, aes(x=0.5, y=diff),  
                   sides="rl", alpha=0.25, color="blue", size=1.75) +
          geom_jitter(data=swha.dat, aes(x=1, y=diff), width = 0.1, height=0,
                      pch=21, bg="white", color="blue", stroke=2) 

  
