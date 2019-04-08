opts_chunk$set(fig.width = 9, fig.height = 6, echo=FALSE, message=FALSE, warning = FALSE, results = 'hide')

#
# START Code Chunk 1


options(width=150)


# END Code Chunk 1

#' Let's get to know each other through data!
#' 
#' 

# START Code Chunk 2
#
survey.d <- read.csv("./data/SurveyResponsesSP19.csv") 
geo.d <- read.csv("./data/UndergradGeo.csv")

#
# END Code Chunk 2

#' We can look at each of these responses individually:

# START Code Chunk 3
  # Bar graphs
      survey.d %>%
        drop_na(degree) %>%
            ggplot(aes(x=reorder(degree,degree, function(x)-length(x)), 
                            fill=factor(degree))) + 
                  geom_bar() +
                    labs(x = "Degree type", y = "Number of students") + 
                    # theme(axis.text.x = element_text(angle = 33, hjust = 1)) +
                    scale_fill_brewer(palette = "Set1", direction = -1) + 
                    theme_bw(14) + 
                    theme(axis.text=element_text(color="black"), 
                          axis.title=element_text(face="bold"),
                          legend.key.width= unit(2, "cm"), 
                          legend.position = "none", 
				panel.grid.major.x = element_blank()) -> degree.gg
  survey.d %>%
    drop_na(program) %>%
      ggplot(aes(x=reorder(program,program, function(x)-length(x)), 
                                     fill=factor(program))) + 
      geom_bar(color="black") +
      labs(x = "Program", y = "Number of students") + 
      # theme(axis.text.x = element_text(angle = 33, hjust = 1)) +
      scale_fill_brewer(palette = "RdYlGn") + 
      theme_bw(14) +  
	theme(axis.text=element_text(color="black"), 
                          axis.title=element_text(face="bold"),
                          legend.key.width= unit(2, "cm"), 
                          legend.position = "none", 
				panel.grid.major.x = element_blank()) -> program.gg 

  grid.arrange(degree.gg, program.gg, ncol=2)
# 
# END Code Chunk 3
  
#
# START Code Chunk 4
# 

#' Or we can combine the factors and show two categorical variables at once:

  survey.d %>%
    drop_na(program, degree) %>%
      ggplot(aes(x=reorder(program,program, function(x)-length(x)), 
                                  fill=factor(degree))) + 
      geom_bar(color="black") +
      labs(x = "Program", y = "Number of students") + 
      # theme(axis.text.x = element_text(angle = 33, hjust = 1)) +
      scale_fill_brewer(palette = "RdYlGn", name="Degree") + 
	theme_bw(14) + 
       theme(axis.text=element_text(color="black"), 
             axis.title=element_text(face="bold"),
             legend.key.width= unit(2, "cm"), 
		panel.grid.major.x = element_blank(), 
            legend.position = "top") 

#' Classic bar debate:
  survey.d %>%
    drop_na(program, water) %>%
      ggplot(aes(x=reorder(water,water, function(x)-length(x)), 
                                 fill=factor(program))) + 
    geom_bar(color="black") +
    labs(x = "Which implies greater water content?", 
         y = "Number of students") + 
    # theme(axis.text.x = element_text(angle = 33, hjust = 1)) +
    scale_fill_brewer(palette = "RdYlGn", name="Degree") + 
    	theme_bw(14) + 
       theme(axis.text=element_text(color="black"), 
             axis.title=element_text(face="bold"),
             legend.key.width= unit(1, "cm"), 
		panel.grid.major.x = element_blank(), 
            legend.position = "right")

#' Also look at data proportionately: 

survey.d %>%
	select(water, degree) %>%
	 drop_na(water, degree) %>%
	group_by(water, degree) %>%
	summarize(count = n()) %>%
	mutate(Count = sum(count), 
		 prop = count/sum(count)) %>%
	ungroup() %>%
	ggplot(aes(	x=water, y=prop, 
			width=Count, fill=degree)) +
	 geom_bar(	stat = "identity", 
			position = "fill", 
			colour = "black") +
  	facet_grid(~water, scales = "free_x", space = "free_x") +
  	scale_fill_brewer(palette = "Set1", direction=-1) +
  	theme_void()

# Map data processing 

  states.md <- map_data("state")
  l48 <- subset(states.md, region !="alaska")
  

  
#' Where you all are from. The bigger the symbol, the more of you are from there:
   geo.d %>% filter(loc == "US") %>%
ggplot() +coord_map() + theme_minimal() + 
    geom_polygon(data=l48, aes(x=long, y=lat, group=group), 
                 color="white", fill="grey90", size=0.25) + 
   stat_sum(data=undergrad.us, aes(x=lon, y=lat, 
                                size=factor(..n..), 
					  fill=degree), 
            geom = "point", pch=24, 
            col="black")  +
   scale_size_discrete(range = c(2, 6), guide=FALSE) + 
    theme(legend.position = "bottom") 

  world.md <- map_data("world")
  world.md <- subset(world.md, region !="Antarctica")
  
 
  
#' Where you all are from. The bigger the symbol, the more of you are from there:
  geo.d %>% filter(loc == "Int") %>%
	ggplot() +coord_quickmap() + theme_minimal() + 
    geom_polygon(data=world.md, aes(x=long, y=lat, group=group), 
                 color="white", fill="grey90", size=0.25) + 
   stat_sum(data=undergrad.int, aes(x=lon, y=lat, 
                                size=factor(..n..), 
					  fill=degree), 
            geom = "point", pch=24, 
            col="black")  +
   scale_size_discrete(range = c(2, 6), guide=FALSE) + 
    theme(legend.position = "bottom") 


  survey.d %>%
    drop_na(relationship) %>% 
    select(relationship) -> relationship
  datCorpus <- Corpus(VectorSource(relationship))
  datCorpus <- tm_map(datCorpus, PlainTextDocument)
  datCorpus <- tm_map(datCorpus, removePunctuation)
  datCorpus <- tm_map(datCorpus, removeWords, stopwords('english'))
  # datCorpus <- tm_map(datCorpus, stemDocument)
  
#' On your relationship with data: 
  wordcloud(datCorpus, scale=c(4,0.5),min.freq=1,max.words=Inf,
            random.order=FALSE, random.color=TRUE)
