install.packages(c("vegan","dplyr")) 
library(vegan)

data(dune)
data(dune.env)

# Load some custom functions

source("https://raw.githubusercontent.com/devanmcg/rangeR/master/R/CustomCommunityFunctions.R")

devtools::install_github("devanmcg/rangeR")


# vegan::capscale offers a metric solution to Multi-Dimension Scaling
# Allows constrained and unconstrained ordination with any distance measure

  (dune.e.mds <- capscale(dune ~ 1, distance="euclidean") ) 

# What is gained with metric MDS vs. non-metric? 

  (dune.e.k2 <- metaMDS(dune, k=2, dist="euc", trace=0) )
  
  scores(dune.e.k2, display="species")[1:10,]
  scores(dune.e.mds, display="species")[1:10,]
  
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
  plot(pro1)
# optimized B-C mMDS vs. Euc mDS (=PCA)
  (pro2 <- procrustes(dune.e.mds, dune.sb.mds) ) # Much more difference (PSS)
  plot(pro2)
  
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
    envfit(dune.cut.mds ~ A1, permutations = 199)
    anova(dune.cap, permutations = 199)
    adonis(dune.Alist ~ A1, permutations = 199)

# Illustrating effect of constrained ordination:
  x11(12,6) ;	par(mgp=c(4, 1, 0), mar=c(6, 6, 1, 1), las=1, mfrow=c(1,2), cex.lab=1.2, cex.axis=1.2)
# Constrained
  plot(dune.cap, main="constrained") 
# Unconstrained
  plot(dune.cut.mds, main="unconstrained") ; plot(envfit(dune.cut.mds ~ A1), p.max=0.1)
  dev.off() 
# View the shifts in site scores
  plot(procrustes(dune.cap, dune.cut.mds) ) 



