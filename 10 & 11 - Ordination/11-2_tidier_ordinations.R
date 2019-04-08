install.packages(c("vegan","tibble","gridExtra")) 
library(vegan)
library(tibble)

data(dune)
data(dune.env)

dune.mds <- capscale(dune ~ 1, distance="bray", metaMDSdist=TRUE, 
                        engine=monoMDS, autotransform=TRUE)

# Clean up plot a bit
# Label only desired species scores, prevent overlap
# Note: identify function *must* be called in x11 environment, 
# *not* Rstudio!

# First, extract scores from MDS: 
dune.scores <- list(species=as.data.frame(scores(dune.mds, display="species")), 
                    sites=as.data.frame(scores(dune.mds, display="sites")))

x11() 
plot(dune.mds, display="species", type="p") 
points(dune.mds, display="species", pch=19, col="red", cex=c(rowSums(dune)*0.1))
spp.sel <- identify(x=dune.scores$species$MDS1, y=dune.scores$species$MDS2,
         labels = rownames(dune.scores$species), col="black")
# Grab this list of species
spp.sel <-dune.scores$species[c(spp.sel),]

# Other gradient visualization
# We first fit an arrow for the vector: 
plot(dune.cut.mds, main="unconstrained") ; plot(envfit(dune.cut.mds ~ A1), p.max=0.1)
# But we can also display the gradient:
ordisurf(dune.cut.mds ~ A1, add=FALSE, labcex = 1.2, bubble=5, main="A1")
plot(envfit(dune.cut.mds ~ A1), p.max=0.1)
# Non-linear:
ordisurf(dune.cut.mds ~ A1, add=FALSE, knots = 2, labcex = 1.2, bubble=5)
ordisurf(dune.cut.mds ~ A1, add=TRUE, knots = 2, family="Gamma", labcex = 1.2, col="darkgreen")

# Extract ord results
  dune.scores <- scores(dune.mds, scaling = 0)
  str(dune.scores)
    # Extract species scores
      dune.spp <- round(data.frame(dune.scores$species),3)
      dune.spp <- tibble::rownames_to_column(dune.spp, var="species")
      head(dune.spp)
    # Extract site scores and combine with env variables
      dune.sites <- data.frame(dune.env,
                               MDS1=round(dune.scores$sites[,1],3),
                               MDS2=round(dune.scores$sites[,2], 3) )
      head(dune.sites)

dune.vec <- as.data.frame(round(scores(envfit(dune.mds,dune.env,  
                                       choices=c(1:2)), 
                                "vectors"),3))
dune.cen <- round(data.frame(envfit(cbind(dune.sites$MDS1, 
                                          dune.sites$MDS2) ~ dune.sites$Management,
                                    choices=c(1,2))$factors$centroids),3)  
dune.cen <- data.frame(management=unique(dune.env$Management),
                        man.cen.1=dune.cen$Dim1, man.cen.2=dune.cen$Dim2)

dune.sites<- merge(x=dune.sites, y=dune.cen, 
                     by.x="Management", by.y="management")
head(dune.sites)


# ggplotting ordination 

ord.gg <- ggplot(data=dune.sites) + theme_bw(16) + 
  labs(x="MDS Axis 1",y="MDS Axis 2") + 
  theme(panel.grid=element_blank(), 
        legend.position="top",
        legend.direction="horizontal")

ord.gg + geom_point(aes(x=MDS1, y=MDS2, 
                        shape=Management, colour=Management), size=2) +

(ord.gg <- ord.gg + geom_polygon(aes(x=MDS1, y=MDS2, fill=Management)) +
           geom_point(aes(x=MDS1, y=MDS2, bg=Management), 
                      colour="black", pch=21, size=3, stroke=2) )
(ord.gg <- ord.gg + geom_segment(data=dune.vec, aes(x=0, y=0, 
                                          xend=(MDS1), 
                                          yend=(MDS2)),
                       arrow=arrow(length = unit(0.03, "npc")),
                       lwd=1.5))
(ord.gg <- ord.gg + geom_text(data=dune.vec, 
                 aes(x=MDS1, y=MDS2, label=rownames(dune.vec)), 
                 nudge_y = -0.03, size=6, fontface="bold") )  
ord.gg + geom_label(data=dune.spp, aes(x=MDS1, y=MDS2, 
                                 label=species), 
              label.padding=unit(0.1,"lines"),
              label.size = 0, fontface="bold")
(ord.gg <- ord.gg + geom_point(data=dune.spp, aes(x=MDS1, y=MDS2), 
                      shape="+", color="grey30", size=5))
  ggplot(data=dune.spp, aes(x=MDS1, y=MDS2)) + theme_bw(16) + 
            labs(x="MDS Axis 1",y="MDS Axis 2") + 
            coord_cartesian() +
            theme(panel.grid=element_blank()) +
                      geom_label(aes(label=species),   
                                 label.padding=unit(0.1,"lines"),
                                 label.size = 0, fontface="bold")

ord.spp <- ggplot(data=spp.sel, aes(x=MDS1, y=MDS2)) + theme_bw(16) + 
  labs(x="MDS Axis 1",y="MDS Axis 2") + 
  coord_cartesian() +
  theme(panel.grid=element_blank()) +
  geom_label(aes(label=rownames(spp.sel)),   
             label.padding=unit(0.1,"lines"),
             label.size = 0, fontface="bold")
x11(12, 6)
gridExtra::grid.arrange(ord.spp, ord.gg + theme(legend.position=c(0.63,0.05)), nrow=1)




