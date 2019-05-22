# NDSU Analysis of Ecosystems
# Devan Allen McGranahan (devan.mcgranahan@ndsu.edu)


	install.packages(c("ggplot2","lme4","car", "MuMIn") )
	options(digits=3)
	
	setwd("E:/r") 
	
	response <- read.csv("./data/fire_response.csv") 
	str(response)
		
	# Take a look at the data in terms of H1: biomass varies with burn season
	library(ggplot2)
  bm.gg1 <- ggplot(data=response, aes(x=burn.season, 
                                      y=biomass)) + theme_bw(16) +
  	          geom_point(data=aggregate(biomass~burn.season, 
  	                            response, FUN=mean), 
  	                      aes(x=burn.season, y=biomass), 
  	                      pch=24, size=5, stroke=2)
  bm.gg1 + geom_boxplot(size=1, alpha=0)
  
  anova(lm(biomass ~ burn.season, data=response) ) 
  
  # Different perspective:
  (bm.gg1 <- bm.gg1 + geom_violin(size=1, alpha=0) )
  bm.gg1 + geom_jitter(aes(color=ranch)) 

  # Could include ranch as predictor variable...
    anova(lm(biomass ~ burn.season + ranch, data=response) )
    # BUT: 
      # - this specifically tests differences between ranches;  
          # Ranches in these data are replicates,
          # not hypothesized predictors of different responses.
      # - violates assumption of independent samples
  
	# Solution: control for ~random~ variation with Mixed-Effect Models
	# Variability across ranches
	 ggplot() + theme_bw(16) +
	   geom_boxplot(data=response, aes(x=burn.season, y=biomass, 
	                                   fill=ranch)) +
	   geom_point(data=aggregate(biomass~burn.season+ranch, 
	                             response, FUN=mean), 
	              aes(x=burn.season, y=biomass, bg=ranch), 
	              pch=24, color="white", size=3, stroke=1.5,
	              position = position_dodge(width=0.75))
	
	 
	 # Variability within ranches
	 ggplot() + theme_bw(16) + facet_wrap(~ranch) + 
	   geom_boxplot(data=response, aes(x=pasture, 
	                                   y=biomass, 
	                                   fill=burn.season)) 
	   
	# Better use a mixed-effect model!
	library(lme4)
	
	resp.lmer <- lmer(biomass ~ burn.season + (1|ranch/pasture), 
	                  data=response, REML=FALSE)
	fixef(resp.lmer)  # Get fixed-effect coefficients. 
	                  # coef() returns random effects
	anova(resp.lmer) 
	
	# Uh oh, no p-value, what gives?
	# Too much to explain, but we have two options: 
	car::Anova(resp.lmer)
	
	#Or we can compare the model against the null:
	resp.null <- lmer(biomass ~ 1 + (1|ranch/pasture), 
	                  data=response, REML=FALSE)
	anova(resp.null, resp.lmer)
	
	# Can also use base confint() to get 95% CIs: 
	confint(resp.lmer)
	
	# Model diagnostic
	plot(fitted(resp.lmer), residuals(resp.lmer), las=1)
	abline(h = 0, lty = 2)
	lines(smooth.spline(fitted(resp.lmer), residuals(resp.lmer)))
	
	# Goodness-of-fit statistic (pseudo R-squared)
	MuMIn::r.squaredLR(resp.lmer)

	