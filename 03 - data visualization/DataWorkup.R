

corals <- read.csv("C:/Users/Devan.McGranahan/GoogleDrive/Teaching/Classes/Analysis of Ecosystems/compiled notes/course materials/3 - data visualization/ClementsHay2019.csv")

c.d <- data.frame( ) 

for(i in 1:length(unique(corals$Treatment))) {
  # i = 1 
  TRT = as.character(unique(corals$Treatment)[i] ) 
  i.d = subset(corals, Treatment== TRT )
   for(j in 1:length(unique(i.d$Species))) {
     # j = 3
     SP = as.character(unique(i.d$Species)[j] ) 
     j.d = subset(i.d, Species == SP)
      for(k in 1:length(unique(j.d$Plot))) {
         #k = 2 
           P = as.character(unique(j.d$Plot)[[k]] ) 
           k.d = subset(j.d, Plot == P)
           k.d$Rep <- k
           c.d <- rbind(k.d, c.d)
           rm(P, k.d)
            }
   }
}

corals <- c.d %>% select(Plot, Rep, CoralID, Species, Treatment, MassChangePrct) %>%
                    arrange(Plot)

write.csv(corals, row.names = FALSE, file="C:/Users/Devan.McGranahan/GoogleDrive/Teaching/Classes/Analysis of Ecosystems/compiled notes/course materials/3 - data visualization/ClementsHay2019.csv")

