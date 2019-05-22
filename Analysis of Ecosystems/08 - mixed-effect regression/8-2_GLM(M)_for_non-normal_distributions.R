install.packages(c("tidyverse","pscl", "car","VGAM", "multcomp", "MuMIn"))

		cat.scanners <- load(file="./data/cat.scanners.Rdata")
		
	  str(cat.scanners)
	
	
		# Visualize as a contingency table 
			# Prepare contingency table
		    (cont.tab <- xtabs(~inside + neighbor, data=cs)) 
			
				# Plot contingency table as a mosaic plot
			plot(cont.tab, col=c("blue","orange"), cex.axis=1.4, las=1, 
			     main="", ylab="Neighbor", xlab="Inside?")
		
		# Test for significance with chi-squared contingency table test 
			chisq.test(cont.tab)
		
		# Can also use linear model framework with GLM	

		  cs.glm1 <- glm(as.factor(inside) ~ neighbor, family=binomial(link="logit"), 
		              data=cat.scanners)

		  anova(cs.glm1, test="Chisq")
		  pscl::pR2(cs.glm1)
		  
		  library(car)
		  Anova(cs.glm1,  type="2", 
		        test.statistic="LR", error.estimate="deviance") 
		  
		# Advantage comes with more complex datasets: 
		  # load, prep data
		  library(VGAM) 
		  data(V1)
		  V1$hits <- as.factor(V1$hits)
		  
		   V1.2 <- V1 %>% 
		          mutate(freq = round(ofreq / sum(ofreq), 3) ) %>%
		         complete(nesting(hits=full_seq(hits, 1)) , fill = list(ofreq=0, freq=0)) 
		            

		  # Check data and distribution
		  (V1.gg <-  ggplot(V1, aes(x=hits, y=ofreq / sum(ofreq))) + theme_bw() + 
		              geom_bar(stat="identity",
		              colour="black", fill="#feb24c")) + 
		              scale_x_continuous(breaks=c(0:7), labels=c(0:7))

		  V1.PMF <- data.frame(dist="Poisson", 
		                        X=V1.2$hits, 
		                        Y = round(dpois(x=V1.2$hits, lambda = 1), 3)) 

  	  V1.gg +	geom_point(data=V1.PMF, aes(x=X, y=Y), 
  	                    stat="identity",pch=22,
  	                    bg="#2b8cbe", col="white", size=4) + 
  	          scale_x_continuous(breaks=c(0:7), labels=c(0:7))
  	  
  	  # Fit a GLM
  	  V1.2$hitsF <- as.factor(V1.2$hits) # Required for multcomp
  	  cs.glm2 <- glm(ofreq ~ 0 + hitsF, family=poisson(link="log"), 
  	                 data=V1.2)
  	  
  	  anova(cs.glm2, test="Chisq")
  	  Anova(cs.glm2,  type="2", 
  	        test.statistic="LR", error.estimate="deviance")
  	  
  	  library(multcomp)
      summary(glht(cs.glm2, linfct=mcp(hitsF ="Tukey")))
      
      # Alternatively, test against 0 by removing intercept: 
      summary(glm(ofreq ~ 0 + as.factor(hits), family=poisson(link="log"), 
                             data=V1))
  	      
#
# General Linear Mixed-effect Models (GLMM)
#
      
(mpg.distr.gg <-  mpg %>%
    ggplot(aes(x=cty)) + theme_bw() + 
    #coord_cartesian(xlim=c(8,38)) + 
    geom_histogram(aes(y=..density..),      
                   binwidth=1,
                   colour="black", fill="lightgreen") +
    geom_density(alpha=.2, fill="#FF6666") )
 
MASS::fitdistr(mpg$cty, "normal")
MASS::fitdistr(mpg$cty, "Gamma")

mpg.distr.gg +
    stat_function(data=mpg, 
                  fun = dnorm, 
                  args=list(mean=16.9,    
                            sd=4.2),    
                  colour="blue", size=1.1) +
    stat_function(data=mpg, 
                  fun = dgamma, 
                  args=list(shape=16.4,    
                            rate=0.97),    
                  colour="darkred", size=1.1) +
    annotate("text", x=c(12, 26), y=c(0.096, 0.04), 
             label=c("Gamma", "Gaussian"),
             color=c("darkred", "blue"), size=8) 

# Fit a Gamma glm 

  cty.d.glm <- glm(cty ~ displ, family=Gamma, mpg)
  summary(cty.d.glm)

  cty.n.glm <- glm(cty ~ 1, family=Gamma, mpg)
  anova(cty.n.glm, cty.d.glm)  # No P-value
  anova(cty.n.glm, cty.d.glm, test = "Chisq")
  Anova(cty.d.glm) 
  
# Fit a Gamma GLMER
 
  starwars 
  URL <- url("https://docs.google.com/spreadsheets/d/e/2PACX-1vQquVMp46Wn_R1bg_dm8shxj-2Wenc3Nk-ndo66k_XztsoEyXe2dKgZIz_YI4fCQMRZFAY7SMHGY2fH/pub?gid=0&single=true&output=csv")
  sw.r <- read.csv(URL)
  
  sw.d <- 
    starwars %>%
    left_join(., sw.r, factor_key="homeworld") %>%
    select(-sector, -system)  %>%
    drop_na(homeworld, region, mass) %>%
    filter(gender %in% c("male","female")) %>%
    select(region, homeworld, gender, height, mass ) %>%
    mutate(region = as.character(region))
  
  sw.d %>%
    ggplot(aes(x=mass)) + theme_bw() + 
    coord_cartesian(xlim=c(10,170)) + 
    geom_histogram(aes(y=..density..),      
                   binwidth=5,
                   colour="black", fill="lightgreen") +
    geom_density(alpha=.2, fill="#FF6666") + 
    stat_function(data=sw.d, 
                  fun = dnorm, 
                  args=list(mean=77,    
                            sd=27),    
                  colour="blue", size=1.1) +
    stat_function(data=sw.d, 
                  fun = dgamma, 
                  args=list(shape=6.9,    
                            rate=0.09),    
                  colour="darkred", size=1.1) +
    annotate("text", x=c(30, 112), y=c(0.013, 0.013), 
             label=c("Gamma", "Gaussian"),
             color=c("darkred", "blue"), size=8)
  
  # Fit candiate models 
  mass.glmer0 <- glmer(mass ~ 1 + (1|region), 
                       family = Gamma(link="log"), data=sw.d)
  mass.glmerH <- glmer(mass ~ homeworld + (1|region), 
                       family = Gamma(link="log"), data=sw.d)
  mass.glmerG <- glmer(mass ~ gender + (1|region), 
                       family = Gamma(link="log"), data=sw.d)
  mass.glmerA <- glmer(mass ~ gender + homeworld + (1|region), 
                       family = Gamma(link="log"), data=sw.d)
  mass.glmerI <- glmer(mass ~ gender * homeworld + (1|region), 
                       family = Gamma(link="log"), data=sw.d)
  
  # Model selection
  cand.mod.names <- c("mass.glmer0", "mass.glmerH", "mass.glmerG", 
                      "mass.glmerA", "mass.glmerI")
  
  cand.mods <- list( ) 
  for(i in 1:length(cand.mod.names)) {
    cand.mods[[i]] <- get(cand.mod.names[i]) }
  
  print(aictab(cand.set = cand.mods, 
               modnames = cand.mod.names) ) 
  
  # Simulate 95% CIs
  n.sims<- 500 
  results<-array(NA,c(n.sims,2))
  colnames(results)<-c("Intercept","gender")
  for (i in 1:n.sims){
    y.sim<-unlist(simulate(mass.glmerG)) 
    suppressMessages(mod.sim<-glmer(y.sim~ gender + (1|region), 
                                    family = Gamma(link="log"), sw.d, 
                                    control = glmerControl(tolPwrss=0.05,
                                                           optCtrl=list(maxfun=1000))) )
    results[i,]<-fixef(mod.sim) 
  } 
  
  # See results
  apply(results,2,mean)
  apply(results,2,median)
  apply(results,2,quantile,prob=c(0.025,0.975))



