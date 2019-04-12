<<<<<<< HEAD
install.packages(c("vegan", "plyr", "dplyr", "tibble", "ggplot2", "devtools", "gridExtra")) 
devtools::install_github("jfq3/ggordiplots")
=======
install.packages(c("vegan", "plyr", "dplyr", "tibble", "ggplot2", "ggordiplots", "gridExtra")) 
>>>>>>> d3574484e7636c7eb7687574d8a6dc68d06bc0b8


source("https://raw.githubusercontent.com/devanmcg/rangeR/master/R/CustomCommunityFunctions.R")

library(vegan)
library(dplyr)

data(BCI)
data(BCI.env)

spp.rm <- bouncer(data=BCI, measure="proportion", level=10)

BCI.Alist <- select(BCI, -one_of(spp.rm))

BCI.mds3 <- metaMDS(BCI.Alist, k=4, distance = "bray", trymax = 50)
plot(BCI.mds3)
text(scores(BCI.mds3, display="species") , 
     labels=rownames(scores(BCI.mds3, display="species")))

# One form of clean-up: Prioritize abundant species
stems <- rowSums(BCI) 

plot(BCI.mds3, type="n", las=1) 
points(BCI.mds3, dis="sp", col="red")
ordilabel(BCI.mds3, dis="species", 
          lab=names(BCI), priority = stems)

# Any ordination is simply data plotted on Cartesian X,Y coordinates, 
# which means we should be able to graph them easily with ggplot. 
# But vegan objects have their own class and we need data.frames.

# This begins with extracting the X,Y data from different 
# components of the ordination:

  head(scores(BCI.mds3, display="sites"))
  head(scores(BCI.mds3, display="species"))

# Get spp names for spp scores
  BCI.spp <- scores(BCI.mds3, display="species") %>%
              as.data.frame() %>%
              tibble::rownames_to_column( var = "Species")

# Here we store scores data.frames in a list:

  BCI.scores <- list(species=BCI.spp, 
                      sites=as.data.frame(scores(BCI.mds3, display="sites")))
  
# First, clean up plot a bit
# Label only desired species scores, prevent overlap
# Note: identify function *must* be called in x11 environment, 
# *not* Rstudio!

x11() 
plot(BCI.mds3, display="species", type="p") 
points(BCI.mds3, display="species", pch=19, col="red")
spp.sel <- identify(x=BCI.scores$species$NMDS1, 
                    y=BCI.scores$species$NMDS2,
                    labels = BCI.scores$species$Species, col="black")
# Make sure to click Stop -> Stop Locator when finished.
dev.off()
# Grab species scores for just these species
spp.sel <- BCI.scores$species[c(spp.sel),]

plot(BCI.mds3, type="n", las=1) 
points(BCI.mds3, dis="sp", col="red")
ordilabel(BCI.mds3, dis="species", 
          lab=spp.sel$Species, priority = stems)



# We can also put it in our list
BCI.scores$spp.sel <- spp.sel

# Abbreviate species names

plot(BCI.mds3)
text(NMDS2 ~ NMDS1, BCI.scores$spp.sel, 
     labels=make.cepnames(BCI.scores$spp.sel$Species))

# Environmental variables

(fit <- envfit(BCI.mds3 ~ UTM.EW + Habitat, BCI.env, choices=c(1:4)) )

BCI.scores$vector <- as.data.frame(round(scores(fit, 
                                       "vectors"),3) )

library(ggordiplots)
gg_ordiplot(BCI.mds3, groups = BCI.env$Habitat, 
            spiders=TRUE, ellipse=FALSE, plot=TRUE)
hab.gg <- gg_ordiplot(BCI.mds3, groups = BCI.env$Habitat, 
                      spiders=TRUE, ellipse=FALSE, plot=FALSE) 

BCI.scores$habitat <-
hab.gg$df_spiders %>% 
  plyr::rename(c("x"="NMDS1", 
                 "y"="NMDS2") ) %>%
  filter(Group != "Swamp", Group != "Young")

# ggplotting ordination 

# The base plot
ord.gg <- ggplot() + theme_bw(16) + 
  labs(x="NMDS Axis 1",y="NMDS Axis 2") + 
  geom_vline(xintercept = 0, lty=3, color="darkgrey") +
  geom_hline(yintercept = 0, lty=3, color="darkgrey") +
  theme(panel.grid=element_blank(), 
        legend.position="top",
        legend.direction="horizontal")

ord.gg + geom_point(data=BCI.scores$habitat, 
                    aes(x=NMDS1, y=NMDS2, 
                        shape=Group, colour=Group), size=2) 

(sites.gg <- ord.gg +   
            geom_segment(data=BCI.scores$habitat, 
                         aes(x=cntr.x, y=cntr.y,
                             xend=NMDS1, yend=NMDS2, color=Group), 
                         size=1.2, show.legend = FALSE) +
           geom_point(data=BCI.scores$habitat, 
                      aes(x=NMDS1, y=NMDS2, bg=Group), 
                      colour="black", pch=21, size=3, stroke=2, 
                      show.legend = FALSE)+
           geom_label(data=BCI.scores$habitat,
                     aes(x=cntr.x, y=cntr.y, 
                         label=Group, color=Group), 
                     fontface="bold", size=4,
                     label.size = 0, 
                     label.r = unit(0.5, "lines"), 
                     show.legend = FALSE)  )

# Add environmental vector 
 (sites.gg <- sites.gg + 
          geom_segment(data=BCI.scores$vector, 
                       aes(x=0, y=0, 
                           xend=(NMDS1)*0.3, 
                           yend=(NMDS2)*0.3),
                       arrow=arrow(length = unit(0.03, "npc")),
                       lwd=1.5) +
          geom_text(data=BCI.scores$vector,
                              aes(x=(NMDS1)*0.9, 
                                  y=(NMDS1)*0.9, 
                                  label=rownames(BCI.scores$vector)), 
                    nudge_x = 0.06, nudge_y=-0.05, 
                    size=6, fontface="bold")   )

# View species scores
sites.gg + geom_point(data=BCI.scores$species, 
                    aes(x=(NMDS1), 
                        y=(NMDS2)), 
                    shape="+", color="grey30", size=5)
# Scale back species scores
(sites.gg <- sites.gg + geom_point(data=BCI.scores$species, 
                    aes(x=(NMDS1)*0.75, 
                        y=(NMDS2)*0.75), 
                        shape="+", color="grey30", size=5) )

# Now it starts to get cluttered: 
sites.gg + geom_label(data=BCI.scores$spp.sel, 
                    aes(x=NMDS1*0.75, 
                        y=NMDS2*0.75, 
                                 label=make.cepnames(Species)), 
              label.padding=unit(0.1,"lines"),
              label.size = 0, fontface="bold")

# Make a separate graph of just species scores
(spp.gg <- ord.gg +
  geom_point(data=BCI.scores$species, 
             aes(x=(NMDS1)*0.75, 
                 y=(NMDS2)*0.75), 
             shape="+", color="grey30", size=5) +
  geom_label(data=BCI.scores$spp.sel, 
             aes(x=NMDS1*0.75, 
                 y=NMDS2*0.75, 
                 label=make.cepnames(Species)), 
             label.padding=unit(0.1,"lines"),
             label.size = 0, fontface="bold") )
  
  
x11(12, 6)
gridExtra::grid.arrange(spp.gg, sites.gg, nrow=1)




