install.packages("pacman")
pacman::p_load(tidyverse, likert) 

load(file="./SurveyData.Rdata")

head(survey.raw)[2:10]

survey.d <- 
  survey.raw %>%
      select(starts_with('ease.ind'))  %>%
          mutate_if(is.integer,as.factor) %>% 
            mutate_all(revalue, 
                   c("1"="Strongly disagree",
                     "2"="Somewhat disagree",
                     "3"="No opinion",
                     "4"="Somewhat agree",
                     "5"="Strongly agree") )  
    
  str(survey.d ) 

# A conventional way to plot data
survey.d2 <-
  survey.d %>%
    gather(key=question, value=response, -overall.impacts) %>%          # long format
      mutate(overall.impacts = factor(overall.impacts,                  # re-order levels
                                      levels=c("impact unstated", 
                                               "not impacted",
                                               "Low", "Medium", 
                                               "High", "Very high") ), 
             response = factor(response,                                # re-order levels
                              levels=c("Strongly disagree",
                                       "Somewhat disagree",
                                       "No opinion",
                                       "Somewhat agree",
                                       "Strongly agree")) ) %>%
      drop_na()                                                         # drop NAs
  
  bar.gg <- ggplot(survey.d2, aes(x=response)) + theme_bw() +
            theme(axis.text.x=element_text(angle=45, hjust=1)) +
            facet_wrap(~question) 
  bar.gg + geom_bar(stat="count") 
  bar.gg + geom_bar(aes(fill=overall.impacts), stat="count")

# Give meaningful labels for questions 
  unique(survey.d2$question)

  questions <- c(
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
      "Energy companies approach landowners aggressively and/or condescendingly", 
      "Overall energy impact")
    
    names(survey.d) <- questions

# Likert analysis on subset of questions
  likert(items=survey.d[1:5], 
               grouping = survey.d[[length(names(survey.d))]]) %>%
    plot() 

# Statistical testing 
  
    q.12b <- filter(survey.d2, question=="ease.ind_12b" & 
                    response!="NA" & 
                   !overall.impacts %in% c("impact unstated",
                                          "not impacted")) %>%
                droplevels() 
          
    (chi.d <- xtabs(~ overall.impacts + response, data=q.12b) )
    plot(chi.d, las=1)
    summary(chi.d)
    
  # Ordered logistic regression - categorical, *ordinal* predictors
  # H0: Responses do not differ by impact level 
  # H1: Higher-impact respondents more likely to agree 
   impact.reg <- MASS::polr(response ~ overall.impacts, data=chi.d, 
                          method = "logistic")
   summary(ord.reg)
   anova(ord.reg)

   impact.null <- MASS::polr(response ~ 1, data=chi.d, 
                             method = "logistic")
   anova(impact.null, impact.reg)

    
  