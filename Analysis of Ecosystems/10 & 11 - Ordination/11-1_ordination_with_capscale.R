install.packages(c("vegan",'vegan3d',"dplyr", "devtools")) 

library(vegan)
data(dune)
data(dune.env)

# Load some custom functions

  # Try this first: 
    devtools::install_github("devanmcg/rangeR")
  # If it doesn't work though, try this:
    source("https://raw.githubusercontent.com/devanmcg/rangeR/master/R/CustomCommunityFunctions.R")


# vegan::capscale offers a metric solution to Multi-Dimension Scaling
# Allows constrained and unconstrained ordination with any distance measure

  (dune.e.mds <- capscale(dune ~ 1, distance="euclidean") ) 
  plot(dune.e.mds)
  
  var.view(dune.e.mds, 10) 

# As with before, any ordination based on 
#  Euclidean distance = PCA (= unconstrained RDA):

  dune.pca <- rda(dune ~ 1, scale=FALSE) 
  var.view(dune.pca, 10)
  var.view(dune.e.mds, 10)
  
# But remember Euclidean distance is not always the best fit. 
# Let's try another, more ecologically-meaningful, distance measure: 

  dune.b.mds <- capscale(dune ~ 1, distance="bray") 
  var.view(dune.b.mds, 10)

# Bray-Curtis optimized with metaMDS random start procedure. 
# This combines the best of metric and non-metric MDS
  dune.sb.mds <- capscale(dune ~ 1, distance="bray", metaMDSdist=TRUE, 
                          engine=monoMDS, autotransform=TRUE) 
  var.view(dune.sb.mds, 10) 

# Comparing ordinations
# "default"  vs. "optimized" Bray-Curtis mDS
  (pro1 <- procrustes(dune.sb.mds, dune.b.mds) ) # little difference (PSS)
  plot(pro1, main="default MDS vs. optimized MDS")
# optimized B-C mMDS vs. Euc mDS (=PCA)
  (pro2 <- procrustes(dune.e.mds, dune.sb.mds) ) # Much more difference (PSS)
  plot(pro2, main="Euclidean MDS vs. optimized BC MDS")
  
# Improving ordination performance 
# Summarize species by overall abundance
  colSums(dune)

# But plain sums aren't a measure of frequency of observations
# or relative abundance across sites. 

# Use custom function spp.cut to identify species to remove
# and select a new data.frame w/out those species:
  spp.rm <- bouncer(data=dune, measure="proportion", level=5, PrintResults = "TRUE")
  library(dplyr)
  dune.Alist <- select(dune, -one_of(spp.rm))

# Re-run ordination
  dune.cut.mds <- capscale(dune.Alist ~ 1, distance="bray", metaMDSdist=TRUE, 
                           engine=monoMDS, autotransform=TRUE) 
  var.view(dune.cut.mds, 10)

# Using capscale as constrained ordination 
  A1 <- with(dune.env, A1)
  (dune.cap <- capscale(dune.Alist ~ A1, distance="bray", metaMDSdist=TRUE, 
                        engine=monoMDS, autotransform=TRUE) )
  # View eigenvalues
    var.view(dune.cap, 6)
    var.view(dune.cut.mds, 5)
  # Hypothesis testing 
    (A1.fit <- envfit(dune.cut.mds ~ A1, permutations = 199, choices=c(1:3)) )
    anova(dune.cap, permutations = 199)
    adonis(dune.Alist ~ A1, permutations = 199)

# Illustrating effect of constrained ordination:
  x11(12,6) ;	par(mgp=c(4, 1, 0), mar=c(6, 6, 1, 1), las=1, mfrow=c(1,2), cex.lab=1.2, cex.axis=1.2)
# Unconstrained
  plot(dune.cut.mds, main="unconstrained") ; plot(A1.fit)
  # Constrained
  plot(dune.cap, main="constrained") 
  dev.off() 
# View the shifts in site scores
  plot(procrustes(dune.cap, dune.cut.mds), main="Constrained vs. unconstrained ordination" ) 
  
  library(vegan3d)
    ordirgl(dune.cap, display = "sites", type = "p",
            ax.col = "white")
   ordirgl(dune.cut.mds, display = "sites", envfit=A1.fit, 
            type = "p", ax.col = "white", arr.col="blue")


