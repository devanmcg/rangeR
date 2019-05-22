if (!require("pacman")) install.packages("pacman")
pacman::p_load(permute, plyr, ggplot2)

# Basic comparison of two groups
  # Introduction to Student's t test
  # Source: http://www.aliquote.org/cours/2012_biomed/biblio/Student1908.pdf

  data(jackal)
  str(jackal)
  
  j.sum <- ddply(jackal, .(Sex), 
                 summarise, 
                 mean=mean(Length), 
                 se=round(sqrt(var(Length)/length(Length)),2))
  j.sum
  
# Checking assumptions
  # No. 1: Distribution 
   ( jn.gg <- ggplot(jackal, aes(x=Length)) + theme_bw() + 
      geom_histogram(aes(y=..density..),      
                     binwidth=1,
                     colour="black", fill="lightgreen") +
      geom_density(alpha=.2, fill="#FF6666") )

   (jn.gg <- jn.gg + stat_function(data=jackal, 
                                  fun = dnorm, 
                                 args=list(mean=mean(jackal$Length),
                                           sd=sd(jackal$Length)),
                                 colour="blue", size=1.1) )

     # Zoom out on distribution: 
      jn.gg + xlim(c(mean(jackal$Length)-15, 
                   mean(jackal$Length)+15))
    
    # The Q-Q plot
      # Base graphics:
        qqnorm(jackal$Length, col="orange", cex=2, pch=19, las=1,
             xlab = "Theoretical Quantiles", 
             ylab = "Sample Quantiles")
        qqline(jackal$Length, distribution = qnorm,
             probs = c(0.25, 0.75), col="blue", lwd=2)
      # in ggplot stat_qq:
        ggplot(jackal, aes(sample=Length)) + theme_bw(14) +
          stat_qq(size=4, color="#43a2ca") +
          stat_qq_line(size=1.5)
      
      # But the assumption applies to each group being compared
      ggplot(jackal, aes(x=Length)) + theme_bw(14) + 
        geom_histogram(aes(y=..density..),      
                       binwidth=1,
                       colour="black", fill="lightgreen") +
        geom_density(alpha=.2, fill="#FF6666") + 
        facet_wrap(~Sex) + 
        xlim(c(mean(jackal$Length)-15, 
               mean(jackal$Length)+15))
    
  # No. 2: Independent samples 
    # Is mostly only deduceable from experimental design. 
    
  # No. 3: Equal sample variance
    # Student's t-test assumes equal variance
    # Welch's t-test is cool with unequal variance
     
    # Here's what Student's theoretically compares:
      # (Note subsetted means and pooled SD)
      
       ggplot(data=jackal, aes(x=Length)) + theme_bw() + 
        xlim(c(mean(jackal$Length)-15, 
                 mean(jackal$Length)+15)) +
          stat_function(data=jackal, 
                        fun = dnorm, 
                      args=list(mean=mean(subset(jackal, 
                                      Sex=="Male")$Length),
                                sd=sd(jackal$Length)),
                      colour="blue", size=1.1) +
         stat_function(data=jackal, 
                       fun = dnorm, 
                       args=list(mean=mean(subset(jackal, 
                                       Sex=="Female")$Length),
                                 sd=sd(jackal$Length)),
                       colour="orange", size=1.1) 
      
    # Check variance in our data (function var() in R:)
       aggregate(Length ~ Sex, jackal, FUN=var)
      
       var.test(Length ~ Sex, jackal, ratio=1)
    
    # Here's what we're actually comparing:
   
      ggplot(data=jackal, aes(x=Length)) + theme_bw() + 
        xlim(c(mean(jackal$Length)-15, 
               mean(jackal$Length)+15)) +
        geom_histogram(aes(y=..density.., fill=Sex),      
                       binwidth=1, colour="black") +
         geom_density(aes(fill=Sex), alpha=.3)
      
    # Calculate our t statistic
      
      # Hard? Hardly! We already have the info:
      j.sum
      
      X1 <- j.sum[1,2]
      X2 <- j.sum[2,2]
      se1 <- j.sum[1,3]
      se2 <- j.sum[2,3]
      
      wel.t <- (X1-X2)/sqrt(se1^2 + se2^2)
      
      # Welch's t test with function t.test(): 
      
        t.test(Length ~ Sex, jackal, var.equal=FALSE)
      
      # student's t test:
      
       t.test(Length ~ Sex, jackal, var.equal=TRUE)
       
     # Compare outcomes under different settings.
       # Scrutinize: 
            # t stat and p-value
            # alternative hypothesis statement in output
         
        # Two-tailed (default)
         with(jackal, t.test(Length[Sex=="Male"], 
                             Length[Sex=="Female"], 
                             alternative="two.sided"))
        # One tailed
         # Order matters, better have a good hypothesis!
         with(jackal, t.test(Length[Sex=="Male"], 
                             Length[Sex=="Female"], 
                             alternative="greater"))
         
         with(jackal, t.test(Length[Sex=="Male"], 
                             Length[Sex=="Female"], 
                             alternative="less"))
  