# package slam requires R > 3.3.1. 
# Need to update? Follow these steps to update in R: 

install.packages("installr")
library(installr)
updateR()
# Do it from Rstudio, its fine. 
# Accept all defaults, just click through. 
# Restart Rstudio. Ensure Opening screen on console reads:
# R version 3.3.2 (2016-10-31) -- "Sincere Pumpkin Patch" or later. 
#
# Install, load packages

install.packages("pacman")

pacman::p_load(devtools, twitteR, ROAuth, RCurl, stringr, tm, ggmap, plyr, dplyr, quanteda, qdap, wordcloud, RQDA) 

# Make a wordcloud 

load("./survey.d.Rdata")

datCorpus <- Corpus(VectorSource(survey.d$relationship))
datCorpus <- tm_map(datCorpus, PlainTextDocument)
datCorpus <- tm_map(datCorpus, removePunctuation)
datCorpus <- tm_map(datCorpus, removeWords, stopwords('english'))
wordcloud(datCorpus, scale=c(4,0.5),min.freq=1,max.words=Inf,
          random.order=FALSE, random.color=TRUE)

# Get some new text data


setup_twitter_oauth(consumer_key="DkCWM47PcESDalqMmLL9mv4tv",
                    consumer_secret="A2fdVWPoMLipQQAmysh05tSWY7bjEZHMG6jih9Ud4yqspyqUYf", 
                    access_token ="89751769-JV3m7b3ZFJvhVGrJLZ3njoYU8yZWQinRLXxviBF46", 
                    access_secret="LpOsq0hjEkux5pyi7fgdTjlFUNqP83yXwrFd7S5FegpSp")




cities <- c("Bismarck, North Dakota", "Fargo, North Dakota", 
            "Minneapolis, Minnesota", "Kansas City, Missouri",
            "Des Moines, Iowa", "New York City, New York",
            "San Fransisco, California", "Houston, Texas")

city.coords<-data.frame(city=cities, round(geocode(cities, messaging=FALSE),2))


N=200  # tweets to request from each query  
S=200  # radius in miles

dapl <- do.call(rbind,lapply(1:length(city.coords$lat), 
                             function(i) searchTwitter('dapl',
                lang="en",n=N,resultType="recent",
              geocode=paste(city.coords$lat[i],city.coords$lon[i],
                            paste0(S,"mi"),sep=","))))

dapl.text <- unlist(sapply(dapl, function(x) x$getText()) ) 
write.table(dapl.text, file="./data/dapl_text.txt")

# Clean up text
  dapl.text2 <- gsub("(f|ht)tp(s?)://(.*)[.][a-z]+", " ", dapl.text)
  dapl.text2 <- removeWords(dapl.text2, stopwords("english"))
  dapl.text2 <- removeWords(dapl.text2, c("htt","http", "https","amp"))
  dapl.text2 <- removeNumbers(dapl.text2)
  dapl.text2 <- tokenize(dapl.text2, removeURL=TRUE, removeTwitter=FALSE,
                         removePunct = TRUE, what = "fasterword", simplify=FALSE)
  corpus <- Corpus(VectorSource(dapl.text2))
  corpus <- tm_map(corpus,tolower)
  #corpus <- tm_map(corpus, removeNumbers)
  #corpus <- tm_map(corpus, removeWords, stopwords('english'))
  corpus <- tm_map(corpus, stripWhitespace)
  corpusPTD <- tm_map(corpus, PlainTextDocument)
  
  col=brewer.pal(6,"Dark2")
  wordcloud(corpusPTD, min.freq=20, scale=c(5,0.8), rot.per = 0.25,
            random.color=T, max.word=100, random.order=F,colors=col)
  
# Coding with RQDA
  RQDA()
  