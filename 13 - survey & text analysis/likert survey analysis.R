install.packages(c("likert","plyr","dplyr","car"))
library(likert)
library(plyr)
library(dplyr)

load(file="./survey.data.Rdata")
 

str(survey.data)
is.na(survey.data) <- !survey.data

# Subset by a single question cateogry
ease.ind.items <- data.frame(overall.impacts=survey.data$overall.impacts, 
                    survey.data[,substr(names(survey.data), 1,8) == 'ease.ind'])
str(ease.ind.items)
# Convert all integer columns to factors
  ease.ind.items <- ease.ind.items %>% mutate_if(is.integer,as.factor)
  str(ease.ind.items) # Notice anything wrong??

# Ensure all columns have same number of levels
  # (Fills gaps in case no one checked a level for one question.
  #  Make sure to read from a column that has all levels!)
  ease.ind.items[2:13] <- as.data.frame(lapply(ease.ind.items[2:13], factor, 
                                       levels=levels(ease.ind.items[,2])) )
  str(ease.ind.items) # fixed

# Rename levels of data to actual answer options
  ease.ind.items[2:13] <- as.data.frame(lapply(ease.ind.items[2:13], 
                                               revalue, 
                                               c("1"="Strongly disagree",
                                                 "2"="Somewhat disagree",
                                                 "3"="No opinion",
                                                 "4"="Somewhat agree",
                                                 "5"="Strongly agree")))
  # Levels for grouping factors
    # Check: 
      levels(ease.ind.items$overall.impacts)
  
    # Rename:
      ease.ind.items[[1]] <- factor(ease.ind.items[[1]],
                                    levels=c("impact unstated", 
                                             "not impacted",
                                             "Low", "Medium", 
                                             "High", "Very high"))
    # Re-check:
      levels(ease.ind.items$overall.impacts)

  # A conventional way to plot data
    # First, some data manipulation. 
      # Must go back to numeric data...
         sub.d <- data.frame(impact.category=ease.ind.items[1], 
                          as.data.frame(lapply(ease.ind.items[2:7], 
                                               as.numeric)))
     # ... and store in "long" format:
         stacked.d <- data.frame(sub.d[1], stack(sub.d[2:7]))
         names(stacked.d)[2:3] <- c("response","question")
         
    # Bar graphs
    bar.gg <- ggplot(data=stacked.d, aes(x=response)) + theme_bw() +
               scale_x_continuous("Response", breaks=c(1:5),
               labels=c("Strongly disagree", "Somewhat disagree",
                                       "No opinion", "Somewhat agree",
                                        "Strongly agree")) +
             theme(axis.text.x=element_text(angle=45, vjust=0.5)) +
             facet_wrap(~question)
    
    bar.gg + geom_bar(stat="count") 
    bar.gg + geom_bar(aes(fill=overall.impacts), stat="count")
    
# Give meaningful labels for questions 
  names(ease.ind.items)
  names(ease.ind.items) <- c("Overall energy impact",
    "Energy companies provide \nstandard terms and don't\ninvite negotiation", 
    "Easement contracts are useful tools for landowners to stipulate terms", 
    "Landownders are offended when money is offered as default solution to grievances or problems",
      "Energy companies lack respect for local traditions of honesty and trust",
      "Lack of respect from energy companies has negative effect on landowner relationships", 
      "Energy companies go beyond what is required to support local programs and infrastructure",
      "Energy companies assume reclamation/mitigation practices from elsewhere will work in Bakken",
      "Money alone can't solve problems; technical support required",
      "Energy companies focus on bottom line & disregard local traditions",
      "Energy companies don't consider existing grazing systems, crop rotations, soil conservation, etc. when planning infrastructure",
      "Energy companies lack regard for local ecosystems, natural resources", 
      "Energy companies approach landowners aggressively and/or condescendingly")


# Likert analysis on subset of questions
  ease.ind.gr.lk <- likert(items=ease.ind.items[2:7], 
                           grouping = ease.ind.items[[1]])
  plot(ease.ind.gr.lk)


# Statistical testing 
  
    q.12d <- subset(stacked.d, question=="ease.ind_12d" & 
           response!="NA" & 
           !overall.impacts %in% c("impact unstated",
                                  "not impacted"))
          
    chi.d <- xtabs(~ overall.impacts + response, data=droplevels(q.12d))
    plot(chi.d, las=1)
    
  # Ordered logistic regression - categorical, *ordinal* predictors
   library(MASS)  
   ord.reg <- polr(response ~ overall.impacts, data=chi.d, 
                   method = "logistic")
   summary(ord.reg)
   car::Anova(ord.reg)
    
  