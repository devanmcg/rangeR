# NDSU Analysis of Ecosystems
# Devan Allen McGranahan (devan.mcgranahan@ndsu.edu)

# Remember ?functionname and ??functionname for getting help!

	# Get set up
	install.packages(c("car", "AICcmodavg", "tidyverse", "GGally")) 

  # Visualize fuel economy by first one, then two predictors:
	library(tidyverse)
	
  	mpg.gg <- ggplot(mtcars, aes(x=hp, y=mpg) ) + theme_bw(14) 
  	mpg.gg + geom_point(size=4, pch=21, stroke=2, 
  	                    bg="#377eb8", color="white") 
  	mpg.gg + geom_point(aes(bg=wt), 
  	                    size=4, pch=21, stroke=2, 
  	                              color="white") 
	
	# Linear models test effects
  	m1 <- lm(mpg ~ hp, mtcars)
  	m2 <- lm(mpg ~ hp + wt, mtcars)
	
	# Determine if models are different:
	  anova(m1, m2)
	
	# Check other variables:
	cont.vars <- data.frame(mtcars[1], stack(mtcars[3:7]))
	colnames(cont.vars)[2:3] <- c("value","predictor")

		ggplot(cont.vars, aes(x=value, y=mpg)) + 
	    theme_bw(14) + 
		  geom_point() + 
	    facet_wrap(~predictor, scales = "free")
	
		library(GGally)
		ggpairs(data=mtcars, columns=c(1,3:7))
		
	# Test for collinearity - are one or more variables
		# correlated and therefore redundant?

	  # Calculate Variable Inflation Factors:
			  # Fit a complete model
		    car::vif(lm(mpg ~ hp + wt + qsec + drat + disp, mtcars))

		    # Aim for all vif < 5.0, ideally < 4.0.
		    # Sequentially drop high VIF term(s), refit:
		    car::vif(lm(mpg ~  hp + wt + qsec + drat, mtcars))
		    car::vif(lm(mpg ~  wt + qsec + drat, mtcars))
		
	# Fit full combination of variables
	 	cars.null <- lm(mpg ~ 1, mtcars) 
  	cars.1a <- lm(mpg ~ wt, mtcars)
  	cars.1b <- lm(mpg ~ qsec, mtcars)
  	cars.1c <- lm(mpg ~ drat, mtcars)
  	cars.2a <- lm(mpg ~ wt + qsec, mtcars) 
  	cars.2b <- lm(mpg ~ wt + drat, mtcars) 
  	cars.2c <- lm(mpg ~ qsec + drat, mtcars) 
  	cars.full <- lm(mpg ~ wt + qsec + drat, mtcars) 
	
	# Standalone AIC values aren't much help: 
	  data.frame(AIC(cars.null), AIC(cars.1a), AIC(cars.2a),  AIC(cars.full) ) 
	
	# Also: With smaller sample sizes (n < 100?), 
	# AICc preferred: 
  		library(AICcmodavg)
  	  data.frame(AICc(cars.null), AICc(cars.1a), AICc(cars.2a),  AICc(cars.full) ) 
	
	# Compare models with information criteria
	  
	  # Construct candidate model set
	  # First store model names in a character string
  	  cand.mod.names <- c("cars.null", "cars.1a", "cars.1b", 
  	                      "cars.1c", "cars.2a", "cars.2b", 
  	                      "cars.2c",  "cars.full")
	 
	   # Define an empty list to put models
	      cand.mods <- list( ) 
	  
	   # This function fills the list by model names
    	  for(i in 1:length(cand.mod.names)) {
    	    cand.mods[[i]] <- get(cand.mod.names[i]) }
	  
	  # Function aictab does the AICc-based model comparison
  	   print(aictab(cand.set = cand.mods, 
  	                modnames = cand.mod.names))
	   
	  # Are top two models different? 
	    anova(cars.2a, cars.full)

  	# Calculate 95% confidence intervals for top 2 models
    	(cars.2a.CIs <- as.data.frame(confint(cars.2a) ) )  
    	(cars.full.CIs <- as.data.frame(confint(cars.full) ) )
	    
	 # Get parameter estimates averaged across all models 
	    terms <- c("(Intercept)", "wt", "qsec", "drat")
	    av.params <- as.data.frame(array(NA,c(length(terms),4)))
	    colnames(av.params)<-c("term","estimate","ciL","ciU")
	   for(i in 1:length(terms)) {
  	    av <- modavg(parm = paste(terms[i]), 
  	                 cand.set = cand.mods, 
  	                 modnames = cand.mod.names)
    	    av.params[i,1] <- terms[i]
    	    av.params[i,2] <- round(av$Mod.avg.beta, 2)
    	    av.params[i,3] <- round(av$Lower.CL, 3) 
    	    av.params[i,4] <- round(av$Upper.CL, 3) }
	    
	    av.params
    
	# Plotting confidence intervals
  	
  	ggplot(subset(av.params, terms != "(Intercept)")) + 
  	         coord_flip() + theme_bw(16) +
  	  geom_hline(yintercept = 0) + 
  	  geom_errorbar(aes(x=term,
  	                    ymin=ciL, 
  	                    ymax=ciU), 
  	 	                width=0.1, size=1, color="#377eb8") +
  	  geom_point(aes(x=term, 
  	                 y=estimate), 
  	             size=4, pch=21, stroke=2, 
  	             bg="#377eb8", color="white") 
  	
#
# Scaling variables 
#
  # Sometimes when the units of measurement vary among columns, 
  # one might want all variables to be on the same scale. 
  # scale() sets mean = 0 & sd = 1 for all variables: 
	sccars <-  
	  mtcars %>%
	    scale() %>%
	      as.data.frame()
	
	  # Doesn't affect VIFs:
	  car::vif(lm(mpg ~ wt + qsec + drat, sccars))
  	car::vif(lm(mpg ~ wt + qsec + drat, mtcars))
  	
  	# Does change value of CIs, 
  	# but not their relationship:
  	confint(lm(mpg ~ wt + qsec + drat, sccars))
  	confint(lm(mpg ~ wt + qsec + drat, mtcars))

#  	
# Simulating parameter estimates and CIs for one model
#
  	# Fit a model
  	  mod <-lm(mpg~wt + qsec, sccars) 
  	
  	# Simulate parameter CIs
    	n.sims<- 1000 # no of simulations
    	results<-array(NA,c(n.sims,3))
    	colnames(results)<-c("Intercept","wt","qsec")
    	for (i in 1:1000){
    	  y.sim<-unlist(simulate(mod))# generate y by simulation
    	  mod.sim<-lm(y.sim~wt+qsec, sccars)
    	  results[i,]<-coef(mod.sim) # storing b0-2 from simulations
    	}
  	
  	# See results
    	apply(results,2,mean)
    	apply(results,2,median)
    	apply(results,2,quantile,prob=c(0.025,0.975))
  	
  	# Make a data frame
    	mod.CIs <- data.frame(t(apply(results,2,quantile,prob=c(0.025,0.975))))
    	mod.CIs <- data.frame(terms=rownames(mod.CIs), apply(results,2,mean), mod.CIs )
    	colnames(mod.CIs) <- c("term", "estimate", "ciL", "ciU")
    	row.names(mod.CIs) <- NULL
    	mod.CIs
  	
  	# Plot CIs, compare effect sizes
    	ggplot(subset(mod.CIs, term != "Intercept")) + 
    	  coord_flip() + theme_bw(20) +
    	  geom_hline(yintercept = 0) + 
    	  geom_errorbar(aes(x=term,
    	                    ymin=ciL, 
    	                    ymax=ciU), 
    	                width=0.1, size=2, color="#377eb8") +
    	  geom_point(aes(x=term, 
    	                 y=estimate), 
    	             size=6, pch=21, stroke=2, 
    	             bg="#377eb8", color="white") +
    	  ggtitle("Effect size on fuel economy")
  	  
  	
  	# Convenience function 
    	confint(mod)
    	mod.CIs[c(1,3:4)]
