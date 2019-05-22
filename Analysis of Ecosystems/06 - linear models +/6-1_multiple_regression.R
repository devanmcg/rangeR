
install.packages(c("ggplot2", "dplyr", "gvlma","gridExtra", "multcomp", "vcd", "RVAideMemoire"))

library(ggplot2) 
library(gvlma)

str(mtcars)

# Need to make some updates for data to work properly:
  library(dplyr)
  mtcars<- mtcars %>% 
    mutate(am  = as.factor(am), 
           am = plyr::revalue(am, c("0" = "auto", "1" = "stick")), 
           cyl =factor(cyl, levels=c("4", "6", "8")) )

# 
#  L i n e a r    R e g r e s s i o n 
#
# View data... what graph type is appropriate?
 ( cars.gg <- ggplot(data=mtcars, aes(x=hp, y=mpg)) + 
    theme_bw(14) + xlim(c(-2,350)) )

# Plot data and fit line
  cars.gg + geom_smooth(method="lm", 
                se=TRUE, color="#2ca25f", 
                alpha=0.2, fill="#2ca25f") + 
            geom_point(color="#2ca25f", size=2)

  # Test the line  
    m1 <- lm(mpg ~ hp, mtcars)
    summary(m1)
    
    summary(gvlma(m1)) # Test assumptions
  
# View ANOVA table
  # Two options:
  anova(m1) # ANOVA table from lm
  summary(aov(mpg ~ hp, mtcars)) # ANOVA results from aov
  
# 
#  M u l t i p l e   L i n e a r   R e g r e s s i o n 
#
# Add a factor
  # View data:
    cars.gg + geom_point(aes(color=am), size=2)
  
  # Fit line: 
    cars.gg + geom_smooth(aes(color=am, fill=am),
                          method="lm",se=TRUE,  alpha=0.2) + 
              geom_point(aes(color=am), size=2)

  # Linear multiple regression model - fit and check:
    m2 <- lm(mpg ~ hp + am, mtcars)
    summary(gvlma(m2))
  
  # View ANOVA table:
    anova(m2)

#
#  A n a l y s i s   o f   V a r i a n c e 
#
# What about continuous vs. categorical variables?
  ggplot(mtcars, aes(x=am, y=mpg)) + geom_boxplot() 
  
  # Fit plot for actual distributions
  am.r <- ggplot(data=mtcars, aes(x=mpg)) + 
            geom_histogram(aes(y=..density.., fill=am),      
                           binwidth=1, colour="black") +
            geom_density(aes(fill=am), alpha=.3) + 
            labs(title="Actual comparison")
 
  # Use function dnorm() to get theoretical densities 
    am.distr <- data.frame()
  
    for(i in 1:length(unique(mtcars$am))) {
      dat1 <- filter(mtcars, am == unique(am)[i])
      dat2 <- dat1$mpg
      mu = mean(dat2)
      sigma = sd(dat2)
      x <- seq(0, 50, length=100) 
      d1 <- data.frame(X = x, Y = dnorm(x, mean=mu, sd=sigma)) 
      var = unique(dat1$am)
      d2 <- data.frame(am = var, d1)
      am.distr <- rbind(am.distr, d2) 
    }

    # Fit theoretical PDF plots
      am.t <- ggplot(data=am.distr) + 
                geom_line(aes(x=X, y=Y, color=am),  
                          size=1.1) +
                coord_cartesian(x=c(0,40)) + 
                ggtitle("Theoretical comparison")  
  
  # Compare actual vs. theoretical 
    gridExtra::grid.arrange(am.r, am.t)
 
  # Can test two groups with a t-test... 
    t.test(mpg ~ am, mtcars) 
  
  # ... and also an F test via Analysis of Variance (ANOVA) 
  # ANOVA is actually a special case of the linear model; use lm() : 
    m3 <- lm(mpg ~ am, mtcars)
    summary(gvlma(m3))
    summary(m3) # Linear regression (t statistic)
    anova(m3)   # ANOVA (F statistic)
  
# What about a three-factor categorical variable??
  
  # Formulate a linear hypothesis:
    ggplot(mtcars, aes(x=cyl, y=mpg)) + geom_boxplot() 
    
    cyl.r <- ggplot(data=mtcars, aes(x=mpg)) + 
                geom_histogram(aes(y=..density.., fill=cyl),      
                               binwidth=1, colour="black") +
                geom_density(aes(fill=cyl), alpha=.3) + 
                ggtitle("Actual comparison")
  
  # Find the theoretical distributions for each factor
  
    cyl.distr <- data.frame()
  
  # Use function dnorm() to get theoretical densities for each factor level
    for(i in 1:length(unique(mtcars$cyl))) {
      dat1 <- filter(mtcars, cyl == unique(cyl)[i])
      dat2 <- dat1$mpg
      mu = mean(dat2)
      sigma = sd(dat2)
      x <- seq(0, 50, length=100) 
      d1 <- data.frame(X = x, Y = dnorm(x, mean=mu, sd=sigma)) 
      var = unique(dat1$cyl)
      d2 <- data.frame(cyl = var, d1)
      cyl.distr <- rbind(cyl.distr, d2) 
        }
    
    cyl.t <- ggplot(data=cyl.distr) + 
                geom_line(aes(x=X, y=Y, color=cyl),  
                          size=1.1) +
                coord_cartesian(x=c(0,40)) + 
                ggtitle("Theoretical comparison")
  
  # Compare theoretical distribution to actual distribution
    gridExtra::grid.arrange(cyl.r, cyl.t)
  
  # Linear model - fit and check:
    cyl.mod.1 <- lm(mpg ~ cyl, mtcars)
    summary(gvlma(cyl.mod.1)) 
  
  # View ANOVA table:
    anova(cyl.mod.1)

# Post-hoc group contrasts 
   # Pair-wise comparisons -- two options...
    # ...base Tukey test:
      cyl.mod.2 <- aov(mpg ~ cyl, mtcars)
      TukeyHSD(cyl.mod.2)
    
    # ...package multcomp: 
      library(multcomp) 
      tukey <- glht(cyl.mod.1, linfct = mcp(cyl = "Tukey"))
      summary(tukey)
    
    # Other options in multcomp: 
      # Test whether each group differs from zero 
      # (remove intercept term with y ~ 0 + x )
        CylNoInt <- lm(mpg ~ 0 + cyl, mtcars)
        comp0 <- glht(CylNoInt)
        summary(comp0)

      # Test whether each group differs from a specific value
      # (Define specific contrasts using level names from lm() object)
        comp15 <- glht(CylNoInt, linfct = (cyl = c("cyl6 = 15", 
                                                   "cyl4 = 15", 
                                                   "cyl8 = 15")))
        summary(comp15)

# What about two categorical variables??? 

  # 2 x 2 contingency table
    vcd::mosaic(~ vs + am, mtcars)
    xtabs(~ vs + am, mtcars)
  
    chisq.test(xtabs(~ vs + am, mtcars)) 
  
  # A factor with >2 levels
    vcd::mosaic(~ cyl + am, mtcars)
    (tab <- xtabs(~ cyl + am, mtcars) )
    
    chisq.test(tab)  
  
 # Post-hoc test for multiple categorical group levels
    RVAideMemoire::chisq.multcomp(tab, p.method =  "none")
    RVAideMemoire::chisq.multcomp(tab, p.method =  "fdr")
  
  
