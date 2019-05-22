install.packages(c("plyr","ggplot2","zoo"))

load(file="./ndawn.raw.Rdata")

# Q: How are air temperature and relative humidity related?

# Let's look at data from one station: 
dickinson <- subset(ndawn.raw, Station.Name=="Dickinson")

ggplot(dickinson, aes(x=Avg.Air.Temp, y=Avg.Rel.Hum)) + geom_point() 

dickinson$Hour <- with(dickinson, (paste(substr(Hour,1,nchar(Hour)-2),
                                         "00","00",sep=":")) )

dickinson$timeStamp <- with(dickinson, strptime(paste(Year,"-",
                                                      Month,"-", 
                                                      Day," ",
                                                      Hour,sep=""), 
                         format="%Y-%m-%d %H:%M:%S", tz="MST")) 
class(dickinson$timeStamp)

gg1 <- ggplot(dickinson, aes(x=timeStamp,y=Avg.Air.Temp)) + geom_point()
gg1
gg1 +  scale_x_datetime(date_breaks="1 month")
gg1 + scale_x_datetime(name="Date", 
                       date_breaks="1 month", 
                       date_minor_breaks="1 week", 
                       date_labels="%b") 

(gg2 <- gg1 + scale_x_datetime(limits=as.POSIXct(strptime(c("2016-04-01 00",
                                                    "2016-04-30 23"), 
                                                  format = "%Y-%m-%d %H"))) )       
gg2 + geom_line(color="blue", size=1)

# Compute and plot rolling averages            
library(zoo)
  temp.zoo<-zoo(dickinson$Avg.Air.Temp,
                dickinson$timeStamp)
  daily.mean <- rollmean(temp.zoo, 24, fill=list(NA, NULL, NA))
  weekly.mean <- rollmean(temp.zoo, I(24*7), fill=list(NA, NULL, NA))
  dickinson2 <- dickinson
  dickinson2$daily.mean.temp <- coredata(daily.mean)
  dickinson2$weekly.mean.temp <- coredata(weekly.mean)
  gg3 <- gg2 %+% dickinson2 
  gg3 +  geom_line(aes(y=daily.mean), color="blue", size=2)
  gg3 +  geom_line(aes(y=weekly.mean), color="blue", size=2)

# Compare two variables over time
  dickinson.FW <- data.frame(timeStamp=dickinson$timeStamp,
                    stack(dickinson[c(9,11)]) )
  colnames(dickinson.FW)[2:3] <- c("value","variable")

  ggplot(dickinson.FW, aes(x=timeStamp, y=value, color=variable)) +
    geom_line(size=2)  + theme(legend.position=c(0.75,0.25)) +
    scale_x_datetime(limits=as.POSIXct(strptime(c("2016-06-01 08",
                                                  "2016-06-02 12"), 
                                                format = "%Y-%m-%d %H")))
  
statewide.turf<-  ddply(ndawn.raw, .(Station.Name, Latitude, Longitude, 
                     Year, Month, Day), summarise, 
                    dailymaxTurf=max(Avg.Turf.Soil.Temp)) 

statewide.turf$dateStamp <- with(statewide.turf, strptime(paste(Year,"-",
                                                      Month,"-", 
                                                      Day," ", 
                                                      sep=""), 
                                                format="%Y-%m-%d")) 

mytheme_main <- theme( panel.background = element_blank(), 
                       panel.grid.major = element_line(colour = "#dddddd"), 
                       axis.ticks = element_line(colour = "#dddddd") )

mytheme_map <- theme(
  panel.background = element_blank(), axis.title.x = element_blank(),
  axis.text = element_blank(), axis.line.x = element_blank(),
  axis.line.y = element_blank(), axis.title.y = element_blank(),
  axis.ticks.x = element_blank(), axis.ticks.y = element_blank() )

statewide.turf$tooltip <- row.names(statewide.turf)

# geom_point_interactive example
gg_point_1 <- ggplot(statewide.turf, aes(x = dateStamp, y = dailymaxTurf, 
                                  color = Station.Name, tooltip = Station.N ) ) + 
  geom_point_interactive(size=3)

# htmlwidget call
ggiraph(code = {print(gg_point_1 + mytheme_main)}, width = 7, height = 6)
ggplot(statewide.turf, aes(x=dateStamp, y=dailymaxTurf, color-Station.Name )) + geom_point() 


pacman::p_load(rvg, ggiraph)
library()