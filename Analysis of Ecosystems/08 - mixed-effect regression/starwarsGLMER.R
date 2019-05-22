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
