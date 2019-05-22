#
# START Code Chunk 1

# package slam requires R > 3.3.1. 
# Need to update? Follow these steps to update in R: 

install.packages("installr")
library(installr)
updateR()
 # Do it from Rstudio, its fine. 
 # Accept all defaults, just click through. 
 # Restart Rstudio. Ensure Opening screen on console reads:
 # R version 3.3.2 (2016-10-31) -- "Sincere Pumpkin Patch" or later. 
#
  # Install, load packages

    install.packages("pacman")
    
    pacman::p_load(googlesheets, maps, rgdal, ggmap, plyr, 
                   maptools, dplyr, ggplot2, grid, broom, 
                   tm, SnowballC, wordcloud, gridExtra)
#
# END Code Chunk 1

# START Code Chunk 2
#
    # Fetch survey data
    gs_auth(new_user = TRUE)
    (my_sheets <- gs_ls()) # View available sheets
    surv <- gs_title("survey responses")
      survey.d <- surv %>% gs_read(verbose = FALSE)
      survey.d <- survey.d[,2:6]
      colnames(survey.d) <- c("degree", "program", "undergrad", 
                              "water", "relationship")
#
# END Code Chunk 2
  

# START Code Chunk 3
  # Bar graphs
    degree.gg <- ggplot(survey.d, aes(x=reorder(degree,degree, function(x)-length(x)), fill=factor(degree))) + geom_bar() +
        labs(x = "Degree type", y = "Number of students") + 
        # theme(axis.text.x = element_text(angle = 33, hjust = 1)) +
        scale_fill_brewer(palette = "Set1") + 
        theme_bw() + 
        theme(axis.text=element_text(size=12), 
              axis.title=element_text(size=12,face="bold"),
              legend.key.width= unit(2, "cm"), 
              legend.text=element_text(size=12), 
              legend.title=element_text(size=12, face="bold"), 
              legend.position = "none") 
    
  program.gg <- ggplot(survey.d, aes(x=reorder(program,program, function(x)-length(x)), fill=factor(program))) + geom_bar() +
      labs(x = "Program", y = "Number of students") + 
      # theme(axis.text.x = element_text(angle = 33, hjust = 1)) +
      scale_fill_brewer(palette = "Set1") + 
      theme_bw() +  
      theme(axis.text=element_text(size=12), 
            axis.title=element_text(size=12,face="bold"),
            legend.key.width= unit(2, "cm"), 
            legend.text=element_text(size=12), 
            legend.title=element_text(size=12, face="bold"), 
            legend.position = "none")
  x11(width = 8, height=4)
  grid.arrange(degree.gg, program.gg, ncol=2)
# 
# END Code Chunk 3
  
#
# START Code Chunk 4
# 
  both.gg <- ggplot(survey.d, aes(x=reorder(program,program, function(x)-length(x)), fill=factor(degree))) + geom_bar() +
    labs(x = "Program", y = "Number of students") + 
    # theme(axis.text.x = element_text(angle = 33, hjust = 1)) +
    scale_fill_brewer(palette = "Set1", name="Degree") + 
    theme_bw() +  
    theme(axis.text=element_text(size=12), 
          axis.title=element_text(size=12,face="bold"),
          legend.key.width= unit(2, "cm"), 
          legend.text=element_text(size=12), 
          legend.title=element_text(size=12, face="bold"), 
          legend.position = "top")
  
  wet.gg <- ggplot(survey.d, aes(x=reorder(water,water, function(x)-length(x)), fill=factor(program))) + geom_bar() +
    labs(x = "Which implies greater water content?", 
         y = "Number of students") + 
    # theme(axis.text.x = element_text(angle = 33, hjust = 1)) +
    scale_fill_brewer(palette = "Set1", name="Degree") + 
    theme_bw() +  
    theme(axis.text=element_text(size=12), 
          axis.title=element_text(size=12,face="bold"),
          legend.key.width= unit(2, "cm"), 
          legend.text=element_text(size=12), 
          legend.title=element_text(size=12, face="bold"), 
          legend.position = "top")
 
   x11(width = 8, height=5)
  grid.arrange(both.gg, wet.gg, ncol=2)
#
# END Code Chunk 3 
  
#
# START Code Chunk 4
  states.md <- map_data("state")
  l48 <- subset(states.md, region !="alaska")
  
  undergrad <- data.frame(degree=survey.d$degree, 
          geocode(c(survey.d$undergrad)) )  
  
 map.gg <- ggplot() +coord_map() + theme_bw() + 
    geom_polygon(data=l48, aes(x=long, y=lat, group=group), 
                 color="white", fill="grey90", size=0.25) + 
   stat_sum(data=undergrad, aes(x=lon, y=lat, 
                                size=factor(..n..)), 
            geom = "point", pch=24, 
            col="black", bg="blue")  +
   scale_size_discrete(range = c(2, 6)) + 
    theme(legend.position = "none") 
 x11() ; map.gg
 #
 # END Code Chunk 4
 
 
 # START Code Chunk 5
 #
  datCorpus <- Corpus(VectorSource(survey.d$relationship))
  datCorpus <- tm_map(datCorpus, PlainTextDocument)
  datCorpus <- tm_map(datCorpus, removePunctuation)
  datCorpus <- tm_map(datCorpus, removeWords, stopwords('english'))
  # datCorpus <- tm_map(datCorpus, stemDocument)
  x11() 
  wordcloud(datCorpus, scale=c(4,0.5),min.freq=1,max.words=Inf,
            random.order=FALSE, random.color=TRUE)
#
# END Code Chunk 5
  