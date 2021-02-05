setwd('~/Desktop/Task_02')
Data <- read.csv('http://jonsmitchell.com/data/beren.csv', stringsAsFactors=F)
write.csv(Data, 'rawdata.csv', quote=F)
length(Data)
nrow(Data)
ncol(Data)
colnames(Data)
head(Data)
Data[1 ,]
Data[2 ,]
Data[1:3 ,]
Data[1:3 , 4]
Data[1:5 , 1:3]
head(Feeds)
Feeds <- which(Data [,9] == 'bottle')
berenMilk <- Data[Feeds ,]
head(berenMilk)
Feeds <- which(Data[,'event'] == 'bottle')
head(Feeds)
Feeds <- which(Data[,'event'] == 'bottle')
head(Feeds)
Feeds <- which(Data$event == 'bottle')
head(Feeds)
dayID <- apply(Data, 1, function(x) paste(x[1:3], collapse='-'))
dateID <- sapply(dayID, as.Date, format = "%Y-%m-%d", origin = "2019-04-18")
Data$age <- dateID - dateID[which(Data$event == 'birth')]
head(Data)
beren2 <- Data
beren3 <- beren2[order(beren2$age) ,]
write.csv(beren3, 'beren_new.csv', quote=F, row.names=FALSE)
Feeds <- which(beren3$event == "bottle")
avgMilk <- mean(beren3$value[Feeds])
angFeed <- tapply(beren3$value[Feeds], beren3$age[Feeds], mean)
varFeed <- tapply(beren3$value[Feeds], beren3$age[Feeds], var)
totalFeed <- tapply(beren3$value[Feeds], beren3$age[Feeds], sum)
numFeeds <- tapply(beren3$value[Feeds], beren3$age[Feeds], length)
cor(beren3$value[Feeds], beren3$age [Feeds])
cor.test(beren3$value[Feeds], beren3$age[Feeds])
berenCor <- cor.test(beren3$value[Feeds], beren3$age[Feeds])
summary(berenCor)
berenANOVA <- aov(beren3$value[Feeds] ~ beren3$caregiver[Feeds])
boxplot( beren3$value[Feeds] ~ beren3$caregiver[Feeds], xlab= "who gave the bottle" , ylab = "amount of milk consumed (oz)" )
par(las=1, mar=c(5 ,5 ,1 ,1) , mgp=c(2, 0.5, 0) , tck=-0.01)
plot(as.numeric(names(totalFeed)) , totalFeed , type="b" , pch=16, xlab=" age in days", ylab="ounces of milk")
abline(h=mean(totalFeed) , lty=2, col='red')
pdf("r02b-totalMilkByDay.pdf", height = 4, width = 4)
par(las=1, mar=c(5 ,5 ,1 ,1) , mgp=c(2, 0.5, 0), tck=-0.01)
plot(as.numeric(names(totalFeed)) , totalFeed, type="b", pch=16, xlab="age in days", ylab=" ounces of milk")
abline(h=mean(totalFeed), lty=2, col='red')
dev.off()
Question 1: There's not enough data to be able to correlate with the variables.
Question 2: The graph is very difficult to interpret because a lot of the data is compiled together due to the fact that there was high intervals on the x-axis.
Self-Quiz: hypothesis= The amount of tylenol Beren takes increases with the amount of molars he gets.
source("http://jonsmitchell.com/code/plotFxn02b.R")
unique(beren3$event)
extra credit
beren4 <- beren3[Naps,]
startHour <- (beren4$start_hour)
startMin <- (beren4$start_minute)
stopHour <- (beren4$end_hour)
stopMin <- (beren4$end_minute)
startHour
startMin
stopHour
stopMin
beren4$sleepTime <- ((stopHour - startHour)*60)+(stopMin-startMin)
beren4
totalNap <- tapply(beren4$sleepTime, beren4$age, sum)
totalNap
par(las=1, mar=c(5, 5, 1,1), mgp=c(2, 0.5, 0), tck=-0.1)
plot(as.numeric(names(totalNap)), totalNap, type="b", pch=16, xlab="age in days", ylab="Naptime in minutes ")
cor.test(beren4$start_hour, beren4$sleepTime)
There is a negative correlation between the two. 