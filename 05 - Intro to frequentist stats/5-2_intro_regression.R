install.packages(c("ggplot2","car","gvlma"))

str(mtcars)

library(ggplot2)

cars.gg <- ggplot(data=mtcars, aes(x=hp, y=mpg)) + 
                theme_bw(16) + xlim(c(-2,350))

# Look at our data 
  # Look like a line should work?
  (cars.gg <- cars.gg + geom_point(color="#2ca25f", size=4, alpha=0.5) )

  ( mu.hp <- mean(mtcars$hp) )
  (mu.mpg <- mean(mtcars$mpg))
  
  (cars.gg <- cars.gg + 
              geom_vline(xintercept=0, 
                         color="black") + 
              geom_vline(xintercept=mu.hp, 
                         color="black", linetype=2) + 
              geom_hline(yintercept=mu.mpg, 
                         color="black", linetype=2) + 
              geom_point(aes(x=mu.hp, y=mu.mpg), 
                         color="black", pch=1, stroke=2, size=4) )
 
  # To determine Y intercept, we need to do some math: 
  
    x <- mtcars$hp
    y <- mtcars$mpg
    reg.d <- data.frame(x, y, xdev=(x-mean(x)), 
                        ydev=(y-mean(y)),
                        xdevydev=((x-mean(x))*(y-mean(y))),
                        xdev2=(x-mean(x))^2,
                        ydev2=(y-mean(y))^2)
    head(reg.d)
    
    # These terms describe error structure, 
      # will be put to use later:
        SP <- sum(reg.d$xdevydev)
        SSx <- sum(reg.d$xdev2)
        SSy <- sum(reg.d$ydev2)
    
    # Calculate regression coefficient, beta(1):
        ( b1 <- SP / SSx )
    
    # Re-arrange equation for line
      # and solve for intercept, i.e. beta(0):
      (b0 <- mean(y) - b1*mean(x))
    
    # Plot intercept and add regression line:
      (cars.gg <- cars.gg + geom_point(x=0, y=b0, color="black", size=4, pch=1, stroke=2)  )
     
      (cars.gg <- cars.gg +  geom_abline(intercept=b0, 
                                         slope = b1, 
                                         color="#2ca25f", size=2) )

    # Turns out this is the same line R regression functions draw:
      cars.gg + geom_smooth(data=mtcars, method="lm", formula=y~x, 
                            se=FALSE, color="white", size=4, linetype=3)
    
  # The line is far from a perfect fit of the data, so
    # how much variation does it describe? 
    
    # Calculate Pearson's correlation coefficient: 
      (r = SP /(sqrt(SSx*SSy)) ) 
      (r.sq = r^2)*100
    # Believe it or not R has a function for this:
      with(mtcars, cor(hp, mpg))
    
  # Great, but is it "significant"?
    # First calculate our t statistic:
      (t.stat = (r*sqrt(30)) / sqrt(1-r.sq)   )
    
    # Then determine the "critical t value"
      # (threshold value of t at alpha given df)
      qt(0.05, 30)
      qt(0.05, 300) # Explore interaction between n and power
      qt(0.01, 30)  # Effects of stricter P standards
      qt(0.001, 30) 
      
    # And R has a function for this, too: 
        with(mtcars, cor.test(hp, mpg))
  
  # R's linear regression function lm does all this and more: 
    car.mod <- lm(mpg ~ hp, mtcars)
    summary(car.mod)  # What do we recognize out of this?
      b0
      b1
      t.stat
      r.sq
 
  # The ANOVA table 
      anova(car.mod)
      
      summary(car.mod)$r.squared
  
  # Before we get too far ahead of ourselves, 
    # we need to validate model assumptions. 
    # Fortunately there is a package that does it for us: 
      
      library(gvlma)
      summary(gvlma(car.mod))
    
    # Looks like we're good here but there are many other ways to check
      library(car)
      qqPlot(car.mod)  # Best: all the residuals line up in a row
                       # Worst: Intervals (broken lines) on one side of line
      ncvTest(car.mod) # Check for non-constant variance; want p > 0.05
  
  
   