install.packages(c("vegan", 'devtools', 'RVAideMemoire'))
devtools::install_github("jfq3/ggordiplots")

library(vegan)
data(dune)
str(dune)

# Ordination with metaMDS

  # Euclidean distance == PCA
    # takes the data + distance measure, or 
    # a distance matrix fit by vegdist()
    # k = number of dimensions to fit 
      dune.e.k2 <- metaMDS(dune, k=2, dist="euclidean")
      dune.e.k2  # in metaMDS, call the object, not summary()
      stressplot(dune.e.k2, las=1, # Goodness-of-fit between the distance
                 main="Euclidean") # between sites in the ordination,
                                   # and their actual dissimilarity 
                                   # in the calculated distance matrix. 
                                   # Less-good fit = more stress

  # 2-dimension NMDS  w/ Bray-Curtis measure
  	dune.b.k2 <- metaMDS(dune, k=2, dist="bray")
  	dune.b.k2
  	stressplot(dune.b.k2, las=1, main="Bray-Curtis") 

  # Compare ordinations with procrustes rotations
  	pro1 <- procrustes(dune.e.k2, dune.b.k2) 
  
    plot(pro1, kind=1, las=1)
    text(pro1) 

    plot(pro1, kind=2, las=1)
  
    protest(dune.e.k2, dune.b.k2, scores="sites") 

  # Got environmental data? Compare dissimilarities to gradients
  	data(dune.env)
  	str(dune.env)
  	round(rankindex(dune.env, dune, c("euc", "bray", "canb"), method="pearson"), 2) 
	
	# Test environmental variables against ordination 
	
  	env.euc <- envfit(dune.e.k2, dune.env, permu=999) 
    	env.euc
  	  env.euc$factors
    	env.euc$vectors

	  env.bray <- envfit(dune.b.k2, dune.env, permu=999)
	    env.bray
	    env.bray$factors
	    env.bray$vectors
	    
	  # Testing multi-level factors
	    library(RVAideMemoire)
	    
	    pairwise.factorfit(dune.b.k2, dune.env$Management, 
	                       nperm = 999, p.method = "none")

  # Compare ordinations by groups, gradient
    x11(12,6) # Check the size manually
    par(mgp=c(4, 1, 0), mar=c(6, 6, 1, 1), las=1, mfrow=c(1,2), cex.lab=1.2, cex.axis=1.2)
    	# Euclidean
    	plot(dune.e.k2, type="t", main="Euclidean") 
    	ordispider(dune.e.k2, groups=dune.env$Management, label=TRUE) 
    	plot(envfit(dune.e.k2 ~ A1, data=dune.env, permu=999), p.max=0.10, col="blue") 
    
    	# Bray-Curtis
    	plot(dune.b.k2, type="t", main="Bray-Curtis") 
    	ordispider(dune.b.k2, groups=dune.env$Management, label=TRUE) 
    	plot(envfit(dune.b.k2 ~ A1, data=dune.env, permu=999), p.max=0.10, col="blue") 
    dev.off() 

  # Other gradient visualization
    	ob <- ordisurf(dune.b.k2 ~ A1, dune.env, add=FALSE, labcex = 1.2, bubble=5)
    	plot(ob)
    	plot(envfit(dune.b.k2 ~ A1, data=dune.env, permu=999), p.max=0.10, col="blue") 
    	summary(ob) 
    	
    	# Non-linear:
    	(ob2 <- ordisurf(dune.b.k2 ~ A1, dune.env, add=FALSE, knots = 2, labcex = 1.2, bubble=5) )
    	summary(ob2)
    	anova(ob, ob2, test="Chisq")
  
  # GGplot the NMDS 
    	library(ggordiplots)
    	gg_ordiplot(dune.b.k2, groups = dune.env$Management, 
    	            kind="se", conf=0.95) 
    	gg_ordiplot(dune.b.k2, groups = dune.env$Management, 
    	            spiders=TRUE, ellipse=FALSE) 
    	
# Non-ordination approaches to multivariate hypothesis testing 
  	# Permutational Multivariate Analysis of Variance (perMANOVA)
  	# has been popular. vegan::adonis is similar:
  	
  	  (dM.ad <- adonis(dune ~ Management, dune.env))
    	round(coef(dM.ad),2)  # Coefficient table, each species response, each Management
  	
  	# Then follow up with a post-hoc pairwise test from RVAideMemoire:
  	  dune.m <- vegdist(dune, "bray")
  	  pairwise.perm.manova(dune.m, dune.env$Management, nperm=999) 
  	
  	# But be careful: adonis is by default a Type I Sum of Squares test:
  	  adonis(dune ~ Manure + Management, dune.env)
  	  adonis(dune ~ Management + Manure, dune.env)
  	
  	# RVAideMemoire provides a Type II method
  	  adonis.II(dune ~ Management + Manure, dune.env) 
  	  adonis.II(dune ~ Manure + Management, dune.env) 
  	  
