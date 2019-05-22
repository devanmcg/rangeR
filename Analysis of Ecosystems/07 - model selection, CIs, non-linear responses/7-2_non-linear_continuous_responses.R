install.packages(c("ggplot2", "segmented"))
#
# Polynomial model fitting
#

# make up some data
  party.people <- data.frame(dude="bro", 
                             beverages=c(1:12), 
                             fun=c(60,70,80,90,99,99,100,100,86,64,35,15) )

# Check it out
  library(ggplot2)
  (p.gg <-  ggplot(data=party.people, aes(x=beverages, y=fun)) + theme_bw(16) + 
              scale_x_continuous("Beverages", breaks=c(1:12), minor_breaks = NULL) + 
              geom_point(aes(bg=dude), size=4, pch=21, stroke=2, 
                          color="black")  )

  p.gg + geom_smooth(se=FALSE, linetype=2)


# Fit a non-linear model
  poly.mod <- lm(fun ~ poly(beverages, 2, raw=TRUE), party.people)
  anova(poly.mod)
  coef(poly.mod)
 
  p.gg + geom_smooth(se=FALSE, linetype=2) + 
         geom_smooth(method="lm", formula=y~poly(x, 2), 
                     color="darkred", se=FALSE) 


# # Fit a multiple non-linear model 
  party.people <- rbind(party, data.frame(dude="hipster", 
                                          beverages=c(1:12), 
                                          fun=c(96,94,92,88,82,79,77,54,25,12,8,0) ))
  
  p.gg <- p.gg %+% party.people # Swap out the data.frame 
  
  (p.gg <- p.gg + geom_smooth(aes(color=dude), 
                              se=FALSE, linetype=2) )  
  p.gg + geom_smooth(aes(color=dude), method="lm", 
                     formula=y~poly(x, 2), size=1.5, se=FALSE)
  
  poly.mod1 <- lm(fun ~ poly(beverages, 2, raw=TRUE), party.people)
  poly.mod2 <- lm(fun ~ poly(beverages, 2, raw=TRUE) + dude, party.people)
  anova(poly.mod1, poly.mod2) # Determine if adding dude term is helpful
  coef(nl.m2)

# Don't overfit!

  dum.dat <- data.frame(x=c(1:12), 
                        y=c(65,25,26,35,75,88,100,95,75,64,35,15) )
  
  (d.gg <-  ggplot(data=dum.dat, aes(x=x, y=y)) + theme_bw(14) + 
            scale_x_continuous(breaks=c(1:12), minor_breaks = NULL) + 
            geom_point(size=3) + 
            geom_smooth(se=FALSE, linetype=2) ) 

  d.gg  + geom_smooth(method="lm", formula=y~poly(x, 2), 
                color="darkred", se=FALSE)
    summary(lm(y~poly(x, 2), dum.dat))$r.squared 

  d.gg  + geom_smooth(method="lm", formula=y~poly(x, 4), se=FALSE)
    summary(lm(y~poly(x, 4), dum.dat))$r.squared 
  
  length(dum.dat$x)
    summary(lm(y~poly(x, 11), dum.dat))$r.squared 

  # Perfect model?? Not so fast... 
    d.gg  + geom_smooth(method="lm", formula=y~poly(x, 11), se=FALSE)
#
# Detecting thresholds 
#
  # Segmented regression 
    rager <- data.frame(dude="Rager", 
                        beverages=c(1:12), 
                        fun=c(45,51,65,80,90,89,95,110,85,20,5,0) )
    
    (r.gg <-  ggplot(data=rager, aes(x=beverages, y=fun)) + theme_bw(16) + 
        scale_x_continuous("Beverages", breaks=c(1:12), minor_breaks = NULL) + 
        geom_point(size=4, pch=21, stroke=2, 
                   color="black", bg="#2b8cbe") )
    
    r.gg + geom_smooth(se=FALSE, linetype=2) + 
           geom_smooth(method="lm", formula=y~poly(x, 2), 
                       color="darkred", se=FALSE)
    
    rage.lm <- lm(fun ~ beverages, rager)
    rage.seg <-segmented(rage.lm)

    r.gg + geom_line(data=data.frame(X = 1:12, 
                                     Y = broken.line(rage.seg)$fit), 
                     aes(x=X, y=Y), 
                     color="darkred", size=1.5)
    rage.seg
    
  # Can also test for differences across a grouping variable (factor)
    
    PreParty <- data.frame(dude="PrePartier", 
                           beverages=c(1:12), 
                           fun=c(95,94,97,98,80,60,40,20,0,0,0,0) )
    NewComers <- rbind(rager, PreParty)
    
    (nc.gg <-  ggplot(data=NewComers, aes(x=beverages, y=fun)) + theme_bw(16) + 
        scale_x_continuous("Beverages", breaks=c(1:12), minor_breaks = NULL) + 
        geom_point(aes(bg=dude), pch=21, stroke=2, 
                   color="black", size=4) )
    
    nc.lm <- lm(log(fun+1) ~ beverages, NewComers)
    nc.seg <-segmented(nc.lm)
    nc.gg + geom_line(data=data.frame(X = 1:12, 
                                     Y = broken.line(nc.seg)$fit), 
                     aes(x=X, y=exp(Y)-1), 
                     color="black", size=1.5)
    
    # Determine if grouping variable is significant
      nc.d.lm <- lm(log(fun+1) ~ beverages + dude, NewComers)
      nc.d.seg <-segmented(nc.d.lm, seg.Z=~beverages, psi = list(beverages=c(2)) )
      anova(nc.seg, nc.d.seg) 
   
    # When grouping variable is significant, 
      # fit segmented models to each factor level
        seg.results <- data.frame() 
        for (i in 1:length(unique(NewComers$dude))) {
           Dude = unique(NewComers$dude)[[i]]
          d = subset(NewComers, dude==Dude)
          d.lm <- lm(fun ~ beverages, d)
          d.seg <-segmented(d.lm, psi=5) 
          psi = round(d.seg$psi[[2]], 0)
          s.res <- data.frame(dude=Dude,
                              X=c(1, psi, 12), 
                              Y=c(broken.line(d.seg)$fit[[1]], 
                                  broken.line(d.seg)$fit[[psi]],
                                  broken.line(d.seg)$fit[[12]] ) )
          seg.results <- rbind(seg.results, s.res)
          rm(Dude,d,d.lm,d.seg,psi,s.res)}
    
    # Plot segments by factor levels
      nc.gg + geom_path(data=seg.results, 
                        aes(x=X, y=Y, color=dude), size=1.5) +
              coord_cartesian(ylim = c(0,110))
    
  # Piecewise regression 
    
    lightweight <- data.frame(beverages=c(1:12), 
                              fun=c(55,75,80,85,25,20,17,10,6,3,1,0) )
    
    (l.gg <-  ggplot(data=lightweight, aes(x=beverages, y=fun)) + theme_bw(16) + 
        scale_x_continuous("Beverages", breaks=c(1:12), minor_breaks = NULL) + 
        geom_point(size=4, pch=21, stroke=2, 
                   color="black", bg="#2b8cbe") )
    
    l.gg +  geom_smooth(se=FALSE, linetype=2) + 
            geom_smooth(method="lm", formula=y~poly(x, 2), 
                        color="darkred", se=FALSE)
    
    # Attempt segmented regression
    lw.lm <- lm(fun ~ beverages, lightweight)
    lw.seg <-segmented(lw.lm, psi=2) 
    
    l.gg + geom_line(data=data.frame(X = 1:12, 
                                     Y = broken.line(lw.seg)$fit), 
                     aes(x=X, y=Y), 
                     color="darkred", size=1.5)

  # DIY piecewise regression function
    
      pwr <- function(X, Y) {
      x = X  
      y = Y
      breaks = x[which(x >= 1 & x <= length(x))] 
      mse = numeric(length(breaks)) 
      for(i in 1:length(breaks)){ 
        piecewise1 <- lm(y ~ x*(x < breaks[i]) +
                           x*(x>=breaks[i])) 
        mse[i] = summary(piecewise1)[6] 
      } 
      mse = as.numeric(mse) 
      # require(ggplot2)
      # qplot(x=breaks, y=mse, size=I(4))
      bp = breaks[which(mse==min(mse))][[1]]
      piecewise2 <- lm(y ~ x*(x < bp) + 
                           x*(x > bp))
      pw.coefs <- round(piecewise2$coefficients, 3) 
      pdf(file=NULL)
      if (is.na(pw.coefs[[2]] + pw.coefs[[5]])) 
      {p1 <- data.frame(x=NA, y=NA)} else 
      {p1 <- curve((pw.coefs[[1]] + pw.coefs[[3]]) + 
                     (pw.coefs[[2]] + pw.coefs[[5]])*x, 
                   add=FALSE, from=1, to=bp) } 
      if (is.na(pw.coefs[[1]] + pw.coefs[[4]])) 
      {p2 <- data.frame(x=NA, y=NA)} else 
      { p2 <- curve((pw.coefs[[1]] + pw.coefs[[4]]) + 
                      pw.coefs[[2]]*x, add=FALSE, from=bp, to=max(x)) } 
      dev.off() 
      coords <- data.frame(X1=c(p1$x[1], p1$x[length(p1$x)]),
                                X2=c(p2$x[1], p2$x[length(p2$x)]), 
                                Y1=c(p1$y[1], p1$y[length(p1$y)]), 
                                Y2=c(p2$y[1], p2$y[length(p2$y)]) )
      stats <- data.frame(Breakpoint=bp, 
                          R.squared=round(summary(piecewise2)$r.squared,2))
      MSE = data.frame(breaks=breaks, mse=mse)
      results <- list(Statistics=stats, Coordinates = coords, MeanSquareError=MSE)
      return(results)
      }
      
      # Run our custom function 
        (pw.results <- with(lightweight, pwr(X=beverages, Y=fun))) 
      
      # Plot the piecewise regression 
        l.gg + 
          geom_path(data=pw.results$Coordinates, 
                    aes(x=X1, y=Y1), size=1.25) +
          geom_path(data=pw.results$Coordinates, 
                    aes(x=X2, y=Y2), size=1.25) +
          geom_vline(xintercept = pw.results$Statistics$Breakpoint, 
                     linetype=2, size=1.25)
          
        # Check that BP=5 is good (has lowest error)
          ggplot(pw.results$MeanSquareError, 
                 aes(x=breaks, y=mse)) + theme_bw(14) +
            scale_x_continuous("Beverage threshold", breaks=c(1:12), minor_breaks = NULL) + 
                 geom_path(color="#2b8cbe", size=1.25) +
                 geom_point(size=4, pch=21, stroke=2, 
                            color="black", bg="#2b8cbe") 

    
