

comm.dat <- read.csv("./data/weird_community_data.csv") 

{
don.dat <- subset(comm.dat, landowner=="donald")
stev.dat <- subset(comm.dat, landowner=="steven")  
}

{
install.packages(c("plyr", "tidyverse", "tidyselect", "GGally",
                   "vegan", "BiodiversityR", "pander"))
library(plyr)
library(tidyverse)
library(tidyselect)
library(vegan) 
library(BiodiversityR)
}

# Create three custom functions:
  # Diversity calculator: 
	ABG.div<-function(x){
		gam<-ncol(x)
		alph<-mean(specnumber(x))
		whit<-gam/alph-1
		lande<-gam-alph
		out<-round(c(alph,whit,lande,gam), 2)
		names(out)<-c("Mean Alpha", "Whittaker's Beta", "Lande's Beta","Gamma")
		return(out) 		}

# Calculate A, B, G diversity for each landscape
	comm.dat %>%
	  ddply(., .(landowner), .fun=ABG.div) 
	
# Calculate A, B, G diversity by habitat types 
	comm.dat %>%
    ddply(., .(habitat), .fun=ABG.div) 

# Calculate and compare various diversity indices for each community
	# Note worker functions:
	      # vegan:specnumber
	      # vegan::diversity
  DivIndices <-
  	comm.dat %>%
  	  select(-transect) %>%
  	   mutate(Richness=specnumber(select(., .data$Janis.Joplin : .data$Nicole.Bogner))  , 
  	          ShannonH=diversity(select(., .data$Janis.Joplin : .data$Nicole.Bogner)) , 
  	          Simpson=diversity(select(., .data$Janis.Joplin : .data$Nicole.Bogner),"simpson"), 
  	          InvSimpson=diversity(select(., .data$Janis.Joplin : .data$Nicole.Bogner),"inv"), 
      	      Evenness=ShannonH/log(Richness) ) %>%
  	  select(-one_of(vars_select(names(.), contains(".") )  ) ) 

# View pairwise relationships between measures w/in one landowner
  DivIndices %>%
    filter(landowner == "donald") %>%
      select(-habitat, -landowner) %>%
    	  GGally::ggpairs( ) + theme_bw() 
	
# Compare diversity measures across habitat types	
  DivIndices %>%
    filter(landowner == "donald") %>%
    gather(key=index, value=value, -landowner, -habitat) %>%
      ggplot() + theme_bw(16) +
    geom_boxplot(aes(x=habitat, y=value)) +
    facet_wrap(~ index, scales="free_y") +
    theme(axis.text.x = element_text(angle=45, color="black", hjust=1))
	
# Test if diversity (e.g., H') differs across groups
  DivIndices %>%
    filter(landowner == "donald") %>%
      lm(ShannonH ~ habitat, .) %>%
	      anova() 
	
# Calculate rank abundance by landowner, habitat type
  # forces BiodiversityR::rankabundance to play along 
  # It is a convenient function if you use it exactly as written 
  # Otherwise, it can be a PITA
	  ra <- data.frame()
	  for(i in 1:length(unique(comm.dat$landowner))){
	    require(dplyr) ; require(BiodiversityR)
	    Ldat <- filter(comm.dat, landowner == unique(landowner)[i]) 
	    for(j in 1:length(unique(Ldat$habitat))) {
	      LHdat <- filter(Ldat, habitat==unique(habitat)[j])
	      cd <- LHdat %>% 
	              select(., .data$Janis.Joplin : .data$Nicole.Bogner) %>%
	                as.data.frame() 
	          ra.d <- as.data.frame(rankabundance(x=cd, 
	                                y=LHdat, 
	                                factor='habitat', 
	                               level=unique(LHdat$habitat) ) ) 
	          ra.d2 <- data.frame(landowner=unique(LHdat$landowner), 
	                              habitat=unique(LHdat$habitat), 
	                              species=rownames(ra.d),
	                              rank=ra.d$rank, 
	                              abundance=ra.d$abundance, 
	                              proportion=ra.d$proportion) 
	          ra <- rbind(ra, ra.d2)
	    }
	  }

# Plot rank abundance by habtiat type for Donald's land 
	  ra %>%
	    filter(landowner =="donald") %>%
  	    ggplot(aes(x=rank, y=proportion, 
  	                       color=habitat)) + theme_bw(18) + 
  	      geom_point(size=3) + geom_line() 
	  
# Compare diversity measures across habitat types	by landowner
	DivIndices %>%
    gather(key=index, value=value, -landowner, -habitat) %>%
	    ggplot() + theme_bw(16) +
        geom_boxplot(aes(x=habitat, y=value, fill=landowner)) +
        facet_wrap(~ index, scales="free_y") +
        theme(axis.text.x = element_text(angle=45, color="black", hjust=1))
	
# Test evenness across landowners, 
	# controlling for non-independence among habitat types
	DivIndices %>%
	  aov(Evenness ~ landowner + Error(habitat), .) %>%
	    summary() 
	
# Plot rank abundance by habitat type for both landowners
	ra %>%
	  ggplot(aes(x=rank, y=proportion, 
	             color=habitat)) + theme_bw(18) + 
	  geom_point(size=3) + geom_line() +
	  facet_wrap(~ landowner)
	
	# Zoom in on top 10 species
	ra %>%
	  filter(rank <= 10) %>%
	  mutate(species = as.character(species)) %>%
	  ggplot(aes(x=rank, y=proportion, 
	             color=habitat)) + theme_bw(18) + 
	  geom_point(size=3) + geom_line(aes(group=habitat)) +
	  facet_wrap(~ landowner, scales = "free_x") +
	  scale_x_continuous(breaks=seq(1,10,1), labels=seq(1,10,1)) +
	  theme(panel.grid.minor.x = element_blank())
	
# View the rankings by proportion
	ra %>%
	  filter(rank <= 10) %>%
	  select(-abundance, -species) %>%
	  spread(rank, proportion) %>%
       pander::pander() 
	
# View the top-ranked species by habitat type
	ra %>%
	  filter(rank <= 3) %>%
	  select(-abundance, -proportion) %>%
	  spread(rank, species) %>%
	  pander::pander()
	
# Get insight into species identity
	  ra %>%
	  mutate(spp.abb = str_replace(species, regex("[.]"), ""), 
	         spp.abb = gsub( "[[:lower:]]", "", spp.abb) ) %>%
	    filter(rank <= 10) %>%
	    ggplot(aes(x=rank, y=proportion, 
	               color=habitat)) + theme_bw(18) + 
	    geom_line(aes(group=habitat)) +
	    geom_label(aes(label=spp.abb)) +
	    facet_wrap(~ landowner, scales = "free_x") +
	    scale_x_continuous(breaks=seq(1,10,1), labels=seq(1,10,1)) +
	    theme(panel.grid.minor.x = element_blank())
