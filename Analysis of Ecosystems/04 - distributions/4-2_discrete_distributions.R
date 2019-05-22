install.packages(c("ggplot2", "VGAM"))
library(VGAM) ; library(ggplot2)

# load data: 
  setwd("E:/R/")
  tows <- read.table('./data/TowHaulCapture.txt', header=TRUE)
  data(V1)

# Binomial distribution
# Generate some random data: 
  x <- rbinom(500, prob = 0.2, size = 12)
  tx <- table(factor(x, levels=0:12))/1000
  tx.d <- as.data.frame(tx)
  colnames(tx.d)[[1]] <- "trials"

# View data
bin.gg <- ggplot(tx.d, aes(x=trials, y=Freq)) + theme_bw() + 
  geom_bar(stat="identity",
           colour="black", fill="#feb24c") 
bin.gg

# Fit a Probability Mass  Function: 
  bin.gg + geom_point(data=transform(data.frame(x=1:13), 
                                    y=dbinom(0:12, size=12, prob=0.2)), 
                     aes(x, y), stat="identity", pch=21,
                     bg="#2b8cbe", col="white", size=3) +
                ggtitle("Binomial distribution")
  
# Negative binomial distribution 
  
  tows$Haulfreq <- with(tows, Hauls/sum(Hauls) ) 
  
  nbi.gg <- ggplot(tows, aes(x=Captures, y=Haulfreq)) +
  geom_bar(stat="identity",
           colour="black", fill="#feb24c") 
  
  length(tows$Hauls)
  MASS::fitdistr(tows$Hauls, "negative binomial")
  
  nbi.gg + geom_point(data=transform(data.frame(x=0:18), 
                                    y=dnbinom(0:18, size=0.161, 
                                              mu=49.83)), 
                     aes(x, y), stat="identity", 
                     color="#2b8cbe", size=3) +
              labs(title="Negative binomial")
  
# Influence of different parameters
  
  ggplot(V1, aes(x=hits, y=ofreq))  + theme_bw() + 
    geom_bar(stat="identity",
             colour="black", fill="lightgreen")
  
  V1$hit.freq <- round(with(V1, ofreq/sum(ofreq)), 3)
  
  hit.freq.gg <- ggplot(V1, aes(x=hits, y=hit.freq))  + theme_bw() + 
    geom_bar(stat="identity",
             colour="black", fill="#feb24c")
  hit.freq.gg
  # Get parameters: 
  
  max(V1$hits)
  MASS::fitdistr(V1$ofreq, "negative binomial")
  
  hit.freq.gg + geom_point(data=transform(data.frame(x=0:7), 
                                          y=dnbinom(0:7, size=0.575,
                                                    prob=V1$hit.freq)), 
                           aes(x, y), stat="identity", pch=22,
                           bg="#2b8cbe", col="white", size=3) +
    geom_point(data=transform(data.frame(x=0:7), 
                              y=dnbinom(0:7, size=0.575,
                                        mu=96)), 
               aes(x, y), stat="identity", pch=21,
               bg="#de2d26", col="white", size=3)
  