setwd("~/Desktop/Evolution/Tasks/Task_08")
library("phytools")
 tree <-read.tree("https://jonsmitchell.com/data/anolis.tre")
 plot(tree, type="fan")
data <- read.csv("https://jonsmitchell.com/data/svl.csv", stringsAsFactors=F, row.names=1)
 dim(data)
class(data)
 svl <- setNames(data$svl, rownames(data))
svl
 Ancestors <- fastAnc(tree, svl, vars=TRUE, CI=TRUE)
par(mar=c(0.1,0.1,0.1,0.1))
 plot(tree, type="fan", lwd=2, show.tip.label=F)
 tiplabels(pch=16, cex=0.25*svl[tree$tip.label])
 # it adds the ancestral states 
 nodelabels(pch=16, cex=0.25*Ancestors$ace)
 obj <- contMap(tree, svl, plot=F)
 plot (obj, type="fan", legend= 0.7*max(nodeHeights(tree)), sig=2, fsize=c(0.7, 0.9))
 fossilData <- data.frame(svl=log(c(25.4, 23.2, 17.7, 19.7, 24, 31)), tip1=c("Anolis_aliniger", "Anolis_aliniger", "Anolis_occultus", "Anolis_ricordii", "Anolis_cristatellus", "Anolis_occultus"), tip2=c("Anolis_chlorocyanus", "Anolis_coelestinus", "Anolis_hendersoni", "Anolis_cybotes", "Anolis_angusticeps", "Anolis_angusticeps"))
 fossilNodes <-c()
nodeN <-c()
{
for (i in 1:nrow(fossilData))
i <- 1
 if (i == 1) {
print(Ancestors)
}
plot(tree, cex=0.5)
for (i in 1:nrow(fossilData))	{
Node <- fastMRCA(tree, fossilData [i,"tip1"], fossilData[i,"tip2"])
	fossilNodes[i] <- fossilData[i, "svl"]
 	nodeN[i] <- Node
 	nodelabels(node=Node, pch=21, bg="red", cex=1.2)
 }
names(fossilNodes) <- nodeN
 Ancestors_withFossils <- fastAnc(tree, svl, anc.states=fossilNodes, CI=TRUE, var=TRUE)
install.packages("geiger")
# 1: 1:82 tips
#2: 100 demensions; 1st of species
#3: in the tips of the tree, or "x" (svl); CI95 can compute 95% confidence intervals on ancestral state estimates
#4: the function re-boots at internal nodes and the contrasting state is at the root each time
#7:increases the ancestral state
# 8-10:
library("geiger")
?fitContinuous
fitContinuous(tree, svl, model='EB')
fitContinuous(tree, svl, model='OU')
fitContinuous(tree, svl, model='BM')


setwd("~/Desktop/Evolution/Tasks/task_10")
install.packages('diversitree')
library('diversitree')
transition_0to1 <- 0.1
transition_1to0 <- 0.1

speciation_0 <-0.2
extinction_0 <- 0.15
speciation_1 <- 0.4
extinction_1 <- 0.1
maxN <-1e3
maxT <- 50
Pars <- c(speciation_0, speciation_1, extinction_0, extinction_1, transition_0to1, transition_1to0)
simTree <- tree.bisse(Pars, max.taxa = maxN, max.t = maxT)

str(simTree)

#?tree.bisse()
stateTable <- table(simTree$tip.state)

stateTable / sum(stateTable)
setwd("~/Desktop/Evolution/Tasks/task_10")

Frequencies <- c('State 0', 'State 1')
Colors <- c('pink', 'blue')

Data <- matrix(c(0.47, 0.48, 0.67, 0.758, 0.631, 0.23, 0.52, 0.5, 0.54, 0.77, 0.77, 0.679), nrow = 3, ncol = 7, byrow=TRUE)

Data

Difference <- c(0.15, 0.1, 0.05, 0.03, 0.02, 0.01)

Freq1 <- c(0.52, 0.5, 0.54, 0.77, 0.77, 0.679)

Freq0 <- c(0.47, 0.48, 0.67, 0.758, 0.631, 0.23)



pdf('Question1.pdf', height =6, width=6)

barplot(Data, main = 'Changes in Frequency of States based on Variation in R Values', xlab = 'Difference in Diversification Rate', ylab = 'Frequency', col=c('pink', 'blue'))


Frequencies <- c('State 0', 'State 1')

Colors <- c('red', 'orange')

Data <- matrix(c(0.93, 0.9, 0.86, 0.85, 0.77, 0.9, 0.947, 0.932, 0.969, 0.944, 0.945, 0.988, 0.934, 0.933, 0.969, 0.989, 0.973, 0.26, 0.2, 0.10, 0.13, 0.27, 0.067, 0.078, 0.081, 0.031, 0.055, 0.045, 0.072, 0.028, 0.039, 0.012, 0.016, 0.022), nrow = 2, ncol = 17, byrow=TRUE)

Data

Difference <- c(0.05, 0.05, 0, 0, 0, 0.1, 0.1, 0.1, 0.2, 0.2, 0.2, 0.3, 0.3, 0.3, 0.45, 0.45, 0.45)

pdf('Question2.pdf', height = 8, width = 8)

barplot(Data, main = 'How Close to Zero State 1 Gets When Transition Rate is Nonzero', xlab = 'Difference in Diversification Rate', ylab = 'Frequencies', col=c('red', 'orange'))





head(Data)

Freq1_Trial1 <- Data[,2]

Freq1_Trial2 <- Data[,5]

Freq1_Trial3 <- Data[,8]

Variance1 <- var(Freq1_Trial1)

Variance2 <- var(Freq1_Trial2)

Variance3 <- var(Freq1_Trial3)

Variance1

Variance2

Variance3

VarianceMatrix <- c(Variance1, Variance2, Variance3)

VarianceMatrix

Trial <- c(1, 2, 3)

pdf('Question3.pdf', height=8, width=8)

barplot(VarianceMatrix, n, main='Variance of Frequency 1 in Each Trial', ylim= c(0, 0.5), xlab='Trial Number', ylab='Variance in Frequencies', col='green')

dev.off()
head(Data)

Freq_0 <- Data[,2]

Freq_0

NDR_0 <- Data[,1]

pdf('OwnTrend1.pdf', height=8, width=8)

plot(NDR_0, Freq_0, xlab='Net Diversification Rate of State 0', ylab='Frequency of State 0', main='How Net Diversification Rate Influences Frequency')

abline(lm(Freq_0~NDR_0), col='blue', lty='dashed')

dev.off()

Freq_1 <- Data[,7]

NDR_1 <- Data[,5]



pdf('OwnTrend2.pdf', height=8, width=8)

plot(NDR_1, Freq_1, xlab='Net Diversification Rate of State 1', ylab='Frequency of State 1', main='How Net Diversification Rate Influences Frequency')

abline(lm(Freq_1~NDR_1), col='blue', lty='dashed')

dev.off()

