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
# Question 1: There's not enough data to be able to correlate with the variables.
# Question 2: The graph is very difficult to interpret because a lot of the data is compiled together due to the fact that there was high intervals on the x-axis.
# Self-Quiz: hypothesis= The amount of tylenol Beren takes increases with the amount of molars he gets.
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
# There is a negative correlation between the two. 
Tyl <- which(beren3$event == "tylenol")
tylData <- beren3[Tyl,]
head( tylData )
totalFeed
unique(tylData$age)
head(tylData)
unique(tylData$age)
DoseDays <- unique(tylData$age)
totalFeed
totalFeed[as.character(DoseDays)]
DoseDays
as.character(DoseDays)
doseFeedNA <- as.character(DoseDays)
na.omit(as.data.frame(x))
doseFeeds <- na.omit(as.data.frame(doseFeedNA))[,1]
NoTyl <- setdiff(names(totalFeed), as.character(DoseDays))
NoTyl
nodoseFeeds <- totalFeed [NoTyl]
nodoseFeeds
doseFeeds
nodoseFeeds
boxplot(doseFeeds, nodoseFeeds)
t.test(doseFeeds, nodoseFeeds)

trueMean1 <- 5
trueSD1 <- 5
population1 <- rnorm(1e6, trueMean1, trueSD1)
trueMean2 <- 4
trueSD2 <- 5
population2 <- rnorm(1e6, trueMean2, trueSD2)
Size <-50
Sample1 <- sample(population1 , Size)
Sample2 <- sample(population2 , Size)
boxplot(Sample1, Sample2)
source("http://jonsmitchell.com/code/simFxn04.R")
MatGrandma <- makeFounder("grandma_mom")
MatGrandpa <- makeFounder("grandpa_mom")
PatGrandma <- makeFounder("grandma_da")
PatGrandpa <- makeFounder("grandpa_da")
Alan <- makeBaby(PatGrandma, PatGrandpa)
Brenda <- makeBaby(MatGrandma, MatGrandpa)
Focus <- makeBaby(Brenda, Alan)
ToMom <- length( grep("mom", Focus))/ length(Focus)
ToMomMom <- length( grep( "grandma_mom", Focus))/ length(Focus)
ToMomDad <- length( grep( "granpa_mom", Focus))/ length(Focus)
Sibling_01 <-makeBaby(Brenda, Alan)
ToSib <- length(intersect( Focus, Sibling_01))/ length(Focus)
ManySiblings <- replicate( 1e3, length( intersect( Focus, makeBaby(Brenda, Alan)))/length(Focus))
quantile(ManySiblings)
mean(ManySiblings)
plot(density(ManySiblings), main="", xlab="proportion shared genes")
HWE <- function(p) {
aa <-p^2
ab <-2*p*(1-p)
bb <-(1-p)^2
return(c(aa=aa, ab=ab, bb=bb))
}
HWE(0.5)
plot (1,1, type="n", xlim=c(0,1), ylim=c(0,1), xlab="freq.allele a", ylab="geno. freq")
GenoFreq <- t(sapply(p, HWE))
lines(p, GenoFreq[,"aa"], lwd=2, col="red")
lines(p, GenoFreq[,"ab"], lwd=2, col="purple")
lines(p, GenoFreq[,"bb"], lwd=2, col="blue")
legend("top",legend=c("aa","ab","bb"),col=c("red","purple","blue"), lty=1, lwd=2, bty="n")
Pop <- simPop(500)
points(Pop[,"freqa"], Pop[, "Genotypes.aa"]/500, pch=21, bg="red")
Pop <-simPop(50)
points(Pop[,"freqa"], Pop[,"Genotypes.aa"]/50, pch=22, bg="red")
install.packages("learnPopGen")
library(learnPopGen)
x <-genetic.drift(Ne=200, nrep=5, pause=0.01)
PopSizes <-5:50
Samples <- rep(PopSizes, 5)

tExt <- sapply(Samples, function(x) nrow(simPop(x, 500)))
Line <- lm(tExt ~ Samples)
summary(Line)
Line$coef
plot(Samples, tExt)
abline(Line)

#questions: 1.)I do not think that they're that different
#2.) 0.5 no it doesn't match what I thought.
#3.) 0.06655
#4.) 0.43345
#5.) The numbers are not equally related 
#6.) The hraph shows inc in geno freq aa inc, ab inc reaches a peak in middle then dec, bb dec
#7.)no it doesn't match bc the graph most are aa and not ab
#8.) not as high of an inc as the other graph . The highest point is the same.


source("http://jonsmitchell.com/code/fxn05.R")
Pop1<- simPop(Popsize = 50, nGenerations = 100, initial_p = 0.5, h =1, s = 0)
plot (1:nrow (Pop1), Pop1 [,1], ylim=c (0,1), type = "l", xlab="generation", ylab="allele freq.", lwd=2)
lines(1:nrow(Pop1), Pop1[,2], lwd=2, col='red')
legend("topleft", legend = c("a", "b"), col = c("black", "red"), lwd = 2, bty="n")
plotFit( nruns = 10, n = 50, ngens = 100, init_p=0.5, h = 1, s = 0 )
Expectation <- c(10, 10, 10, 10)
Observed <- c(15, 15, 5, 5)
Chisq <- sum(((Expectation-Observed)^2)/Expectation)
barplot(rbind(Expectation, Observed), beside=T, main=bquote(chi^2 ~ "=" ~.(Chisq)), legend.text=c("expected", "observed"))
results<- read.csv("http://jonsmitchell.com/data/biol112labresults.csv", stringsAsFactors=F)
counts<- results[,c("yellow","red","green","blue","black","tan")]
backgrounds <- c("White" ,"Red" ,"Yellow" ,"Green" ,"Blue" ,"Black")
backgroundCol <- c ("white", "#d53e4f", "#fee08b", "#abdda4", "#3288bd", "black")
calcChi(counts[1,])
Chisqs <- apply(counts, 1, calcChi)
plotChis(counts)
Avg <- mean(Chisqs)
backgroundAvgs <- tapply(Chisqs, results[,3], mean)
propSig <- length( which( Chisqs > 11.70))/length(Chisqs)
percSig <- round(100 * propSig)
par(las = 1, mar = c(4, 4, 1, 1), mgp = c(2, 0.5, 0), tck = -0.01, cex.axis=1)
hist(Chisqs, main="", xlab="chi-squared values", ylab="frequency")
par(las=1, mar=c(4, 4, 1, 1), mgp=c(2, 0.5, 0), tck = -0.01, cex.axis=1)
plot(1, 1, xlim=c(0, 400), ylim=c(1, 8.5), xlab="", ylab="", type="n", yaxt="n")
axis(2, at = 1:length(backgrounds), labels = backgrounds)
mtext(side=1, expression(chi^2), cex=1.75, line=2.5)
counter <- 1
for (i in backgrounds){
Data <- Chisqs[which(results[,3] == i)]
addHist(Y=counter, Dat=Data, Color=backgroundCol[counter])
counter <- counter +1
}
abline( v = 11.70, lty=2, lwd=2, col='black')
#no
Simulation <- simDraws(10000)
addHist(Y=7, Dat=Simulation, Color="lightgray")
mtext(side=2, at=7, line=0, "simulated")
abline(v=11.70, lty=2, lwd=2)
Fit<- c(1, 1, 1, 1, 1, 1)
names(Fit)<- 1:6
Simulation2<- simDraws(1e4, w=Fit)
addHist(Y=8, Dat=Simulation2, Color=rgb(0,0,0,0.25))
Fit <- c(0.1, 1, 1, 1, 1, 1)
names(Fit)<- 1:6
Simulation3 <- simDraws(1e4, w=Fit)
addHist(Y=8, Dat=Simulation3, Color=rgb(0,0,0,0.25))
Fit <- c(0.5, 0.6, 0.7, 1, 1, 1)
names(Fit)<- 1:6
Simulation4 <- simDraws(1e4, w=Fit)
addHist(Y=8, Dat=Simulation4, Color=rgb(0,0,0,0.25))
Fit <- c(0.1, 0.2, 0.3, 0.4, 0.5, 1)
names(Fit) <- 1:6
Simulation5 <- simDraws(1e4, w=Fit)
addHist(Y=8, Dat=Simulation5, Color=rgb(0,0,0,0.25))
Fit<- c(0.1, 0.1, 0.1, 0.1, 0.1, 1)
names(Fit)<- 1:6
Simulation6<- simDraws(1e4, w=Fit)
addHist(Y=8, Dat=Simulation6, Color=rgb(0,0,0,0.25))
mtext(side=2, at=8, line=0, "sel.sim")
Simulation7 <- c(Simulation2, Simulation3, Simulation4, Simulation5, Simulation6)
addHist(Y=8, Dat=Simulation7, Color=rgb(0,0,1,0.25))
questions: 
