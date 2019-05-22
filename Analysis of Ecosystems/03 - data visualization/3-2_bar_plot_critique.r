# An introduction to R programming
# NDSU Analysis of Ecosystems
# Devan Allen McGranahan (devan.mcgranahan@ndsu.edu)

# Lesson 3: Introduction to graphing in R, continued
# More basic plotting with plot() + introducing ggplot

##
## S T A R T  H E R E

# Install and load packages for use in this session 

  install.packages(c("tidyverse", "plyr"))
  library(plyr) # Always load plyr before tidyverse/dplyr
  library(tidyverse)

# Today we load data a different way - from a pre-defined workspace. 
# It loads the same way as any other R object:

# Set your working directory: 
  setwd("E:/R/")

# Load R object: 
  load(file="./data/data_visualization.RData")
  # or
  load(file.choose())
  # or find it in the file list in Rstudio (lower right corner)
  
# This is more than just a single R object, it is an entire R session
# and includes all the objects from the session it saved. 
  # To see objects from this session, run 
    ls() 
    
# Data today come from the Weissengerber et al. (2015) paper
    # which illustrated two main problems with barplots. 
    # I digitized data from Figs. 1 and 2 to play with them here. 
    
# Figure 1 
    # check out data from Fig 1
    str(fig1)
    
    # Behold the evil dynamite plot: 
  dyn.plot1 <- 
      ddply(fig1, .(treatment, scenario),
                  summarise, 
                  Mean = mean(response), 
                  se   = sd(response) / sqrt(length(response)) ) %>%
      ggplot(aes(x=treatment, 
                 y=Mean)) + 
            geom_bar(fill="grey", stat="identity") + theme_bw() + 
            geom_errorbar(aes(ymin=Mean - se, 
                              ymax=Mean + se), width=0.3) +
            facet_wrap(~scenario) 
  dyn.plot1
  
    # All seem equal until we look at patterns in data: 
    dyn.plot1 + geom_jitter(data=fig1, # Different dataset? Need data=
                            aes(x=treatment, 
                                y=response), 
                  width=0.25, shape=21, size=3, 
                  col="white", bg="black")
    
    # Plot with different geoms: 
    fig1.gg <- ggplot(fig1, aes(x=treatment, 
                                y=response)) + 
                theme_bw() + facet_wrap(~scenario)
    fig1.gg + geom_boxplot()  
    fig1.gg + geom_violin() 
    fig1.gg + geom_violin() + geom_jitter(width=0.25, shape=21, 
                                          size=3, col="white", bg="black")
# Figure 2 
    
  dyn.plot2 <-
      ddply(fig2, .(period, scenario),
                  summarise, 
                  Mean = mean(response), 
                  se   = sd(response) / sqrt(length(response)) )  %>%
      ggplot(aes(x=period, 
                 y=Mean)) + 
        geom_bar(fill="grey", stat="identity") + theme_bw() + 
        facet_wrap(~scenario) + 
        geom_errorbar(aes(ymin=Mean - se, 
                          ymax=Mean + se), width=0.3) 
  
    dyn.plot2 + geom_jitter(data=fig2, aes(x=period, 
                                           y=response), 
                            width=0.25, shape=21, size=3, 
                            col="white", bg="black")
    # Nothing appears to be amiss...
    # what is wrong with how these data are represented?
    
    dyn.plot2 + geom_point(data=fig2, aes(x=period, 
                                          y=response), 
                            shape=21, size=3, col="white", bg="black") + 
                geom_line(data=fig2, aes(x=period, 
                                         y=response, 
                                         group=replicate)) 
    
    fig2.gg <- ggplot(fig2, aes(x=period, 
                                y=response)) + 
                theme_bw() + facet_wrap(~scenario)
    fig2.gg + geom_point(shape=21, size=3, 
                         col="white", bg="black") + 
              geom_line(aes(group=replicate)) 
    
    # Consider another variable?
    fig2c.gg <- ggplot(fig2, aes(x=period, 
                                 y=response, 
                                 color=conditional)) + 
                  theme_bw() + 
                  facet_wrap(~scenario)
    fig2c.gg + geom_point(shape=19, size=3) + 
               geom_line(aes(group=replicate))

    # Summarize
    
    sum2c <- ddply(fig2, .(period, scenario, conditional),
                  summarise, 
                  Mean = mean(response), 
                  se   = sd(response) / sqrt(length(response)) ) 

    
    ggplot() + theme_bw() + facet_wrap(~scenario) +
        geom_point(data=fig2, aes(x=period, 
                                  y=response, 
                                  color=conditional), 
                   shape=19, size=3, alpha=0.3) + 
        geom_line(data=fig2, aes(x=period, 
                                 y=response, 
                                 color=conditional, 
                                 group=replicate), 
                  alpha=0.3) +
        geom_point(data=sum2c, aes(x=period, 
                                   y=Mean, 
                                  color=conditional), 
                   shape=19, size=5, 
                   position = position_dodge(0.2) ) + 
        geom_errorbar(data=sum2c, aes(x=period, 
                                      ymin=Mean - se, 
                                      ymax=Mean + se, 
                                      color=conditional),
                      width=0.2, size=1.5, 
                      position = position_dodge(0.2)) + 
        geom_line(data=sum2c, aes(x=period, 
                                  y=Mean, 
                                 color=conditional, 
                                 group=conditional), 
                      size=1.5, position=position_dodge(0.2))
    
  # One solution: plot as differences
    
    # Need before-after values side-by-side per replicate. 
    # Currently "gathered" in a single column; must "spread"
    head(fig2) 

    diff.d <- fig2 %>% spread(period, response) %>%
                        plyr::rename( c("1"="before", 
                                        "2"="after"))
    head(diff.d)
    
    diff.d <- diff.d %>%
                mutate(diff = after - before) %>% # Calculate the difference
                  select(-before, -after) # drop unnecessary columns
    head(diff.d)
    
    diff.gg <-
      ddply(diff.d, .(conditional, scenario),
            summarise, 
            Mean = mean(diff), 
            se   = sd(diff) / sqrt(length(diff)) )  %>%
      ggplot(aes(x=conditional)) + 
      geom_hline(aes(yintercept=0), col="black", linetype=3) +
      geom_errorbar(aes(ymin=Mean - se, 
                        ymax=Mean + se, 
                        color=conditional),                     
                    size=1.25, width=0.2) +
      geom_point(aes(y=Mean, 
                     fill=conditional), 
                 shape=21, size=4,
                 color="black", stroke=1.5) +
      labs(x = "Condition", 
           y = "Difference (mean +/- s.e.)") +
      facet_wrap(~scenario) + 
      theme(legend.position = "none", 
            panel.grid.major.x = element_blank())
    diff.gg

    (diff.gg <- diff.gg + geom_jitter(data=diff.d, aes(x=conditional, 
                                           y=diff, 
                                           color=conditional), 
                            width=0.1, shape=1, size=3) )
    
    diff.gg + geom_violin(data=diff.d, aes(y=diff, 
                                           color=conditional), 
                                      fill=NA, trim=FALSE) 

    

    
