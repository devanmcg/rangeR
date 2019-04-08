# Calculating 95% CIs
  
  norm1 <- data.frame(x=seq(-50,100, length.out=1000), 
                      y=rnorm(1000, mean=25, sd=20) ) 
  
  gg.norm <- ggplot() + theme_bw(16) +
    geom_vline(xintercept=0, size=1.5, 
               linetype=1, col="darkgrey") +
                  geom_line(data=norm1, aes(x=x, 
                            y=dnorm(x, mean=25, sd=20)),
                             color="blue", size=2) 
  
  
  norm1.ci <- data.frame(t(round(quantile(norm1$y, 
                        prob=c(0.025,0.975),na.rm=TRUE),2)))
  colnames(norm1.ci) <- c("lower", "upper")
  norm1.ci 
  
  (gg.norm <- gg.norm + geom_errorbarh(data=norm1.ci, 
                                       aes(xmin=lower, xmax=upper, 
                                           y=0.015, x=25), 
                         color="blue", alpha=0.5, size=1, height=0.001) )
  
  norm2 <- data.frame(x=seq(-50,100, length.out=1000), 
                      y=rnorm(1000, mean=25, sd=10) ) 
  
  (gg.norm <- gg.norm + ylim(0,0.04) +
    geom_line(data=norm2, aes(x=x, y=dnorm(x, mean=25, sd=10)),
              color="orange", size=2) )
  
  norm2.ci <- data.frame(t(round(quantile(norm2$y, 
                      prob=c(0.025,0.975),na.rm=TRUE),2)))
  colnames(norm2.ci) <- c("lower", "upper")
  norm2.ci 
  
  gg.norm + geom_errorbarh(data=norm2.ci, aes(xmin=lower, xmax=upper, 
                                            y=0.015, x=25), 
                         color="orange", alpha=0.95, size=2, height=0.002)

# Careful about *support* of distributions:

  gam1 <- data.frame(x=seq(0,100, length.out=1000), 
                      y=rgamma(1000, shape=2, rate=0.1) ) 
  
  gg.gam <- ggplot(data=gam1) + theme_bw(16) +
    geom_line(aes(x=x, y=dgamma(x, shape=2, rate=0.1)),
              color="blue", size=2) +
    geom_vline(xintercept=0, size=1.5, 
               linetype=2, col="darkgrey")
  
  gam.ci <- data.frame(t(round(quantile(gam1$y, 
                            prob=c(0.025,0.975),na.rm=TRUE),2)))
  colnames(gam.ci) <- c("lower", "upper")
  
  gg.gam +geom_errorbarh(data=gam.ci, aes(xmin=lower, xmax=upper, 
                                            y=0.025, x=mean(gam1$y)), 
                         color="blue", alpha=0.5, size=1, height=0.001)

# Parameter estimates and CIs
  mod <-lm(mpg~hp+wt, mtcars) 
  
  library(car)
  Anova(mod, type="2")

# Simulate parameter CIs
    n.sims<- 1000 # no of simulations
    results<-array(NA,c(n.sims,3))
    colnames(results)<-c("Intercept","hp","wt")
        for (i in 1:1000){
          y.sim<-unlist(simulate(mod))# generate y by simulation
          mod.sim<-lm(y.sim~hp+wt, mtcars)
          results[i,]<-coef(mod.sim) # storing b0-2 from simulations
        }

# See results
  apply(results,2,mean)
  apply(results,2,median)
  apply(results,2,quantile,prob=c(0.025,0.975))

  # Make a data frame
  mod.CIs <- data.frame(t(apply(results,2,quantile,prob=c(0.025,0.975))))
  mod.CIs <- data.frame(terms=rownames(mod.CIs), mod.CIs)
  colnames(mod.CIs)[2:3] <- c("lower", "upper")
  row.names(mod.CIs) <- NULL
  mod.CIs

  # Plot CIs, compare effect sizes
  ggplot(subset(mod.CIs, terms != "Intercept")) + 
    coord_flip() + theme_bw(16) +
    geom_hline(yintercept = 0) + 
    geom_errorbar(aes(x=terms,
                      ymin=lower, ymax=upper), 
                  width=0.1, size=1, color="blue")

  # Convenience function 
    confint(mod)
    mod.CIs
