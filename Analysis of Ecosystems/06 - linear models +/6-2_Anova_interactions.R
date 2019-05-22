install.packages(c("plyr","car","ggplot2"))

katsack.sales <- read.csv(file="./data/katsack.sales.csv")

library(plyr)
library(ggplot2)

# On the importance of variable class

  cyl.gg <- ggplot(mtcars, aes(x=cyl, y=mpg, group=cyl)) + theme_bw(16)
  
  cyl.gg + geom_point()
  cyl.gg + geom_boxplot()
  
  cont.m <- lm(mpg ~ cyl, mtcars)
  anova(cont.m)
  summary(cont.m)$coefficients # Pull out pertinent part of summary(lm())
  
  mtcars$cylF <- as.factor(mtcars$cyl) 
  
  fac1.m <- lm(mpg ~ cylF, mtcars)
  anova(fac1.m)
  summary(fac1.m)$coefficients
  
# Are all ANOVAs created equal? 

  ToothGrowth$dose <- as.factor(ToothGrowth$dose)
  
  ggplot(ToothGrowth, aes(x=supp, y=len, fill=dose)) + 
    geom_boxplot(size=1.1) 
  
  anova(lm(len ~ supp + dose, ToothGrowth))
  anova(lm(len ~ dose + supp, ToothGrowth))

  ggplot(mtcars, aes(x=hp, y=mpg, 
                     color=cylF)) + 
    geom_smooth(aes(fill=cylF), method="lm", size=1.1) +
    geom_point(aes(shape=cylF), size=3)
  
  anova(lm(mpg ~ hp + cylF, mtcars) ) 
  anova(lm(mpg ~ cylF + hp, mtcars) ) 

  # Hint:
    count(ToothGrowth, .(supp, dose))
    count(mtcars, .(cylF))


# 'Force' R to use Type II Sums of Squares: 
  library(car) 
  Anova(lm(mpg ~ hp + cyl, mtcars), type = "2") # type sets type of SS
  Anova(lm(mpg ~ cyl + hp, mtcars), type = "2") # order doesn't matter

  
# Moving on: Interaction terms 
 # Summarize data 

    katsack.stats <- ddply(katsack.sales, .(drugs, age, city), 
                           summarize, 
                            mean=round(mean(sales),2), 
                            se=round(sd(sales)/sqrt(length(sales)),2))
  # Plot data 
    ks.gg <- ggplot(katsack.stats, aes(x=drugs, y=mean, 
                                       color=age, shape=age)) + 
              facet_wrap(~city) + theme_bw(18) +
              labs(x="Buyer on drugs?", 
                   y="Katsack sales ($/order)") +
              scale_shape_discrete(name="Buyer\nage") +
              scale_color_discrete(name="Buyer\nage")
 
    ks.gg + geom_line(aes(group=age), 
                      size=1.25, alpha=0.45) + 
                     geom_errorbar(aes(ymax=mean+se, 
                                       ymin=mean-se), 
                                   width=0.2, size=1.25) + 
            geom_point(size=4)

    count(katsack.sales, .(drugs, age, city))
    
  # All three Sums of Squares tests (stats::anova would be fine)
    Anova(lm(sales ~ drugs * age, subset(katsack.sales, city=="Bismarck")), type="2")
    Anova(lm(sales ~ drugs * age, subset(katsack.sales, city=="Fargo")), type="2")
    Anova(lm(sales ~ drugs * age, subset(katsack.sales, city=="Minot")), type="2")
