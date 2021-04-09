install.packages("learnPopGen")
library("learnPopGen")
install.packages("coala")
library("coala")
install.packages("phytools")
library("phytools")
model<- coal_model(sample_size = 5, loci_number = 10, loci_length = 500, ploidy = 2) +
feat_mutation(10) +
feat_recombination(10) +
sumstat_trees() +
sumstat_nucleotide_div()
stats<- simulate(model, nsim = 1)
Diversity<- stats$pi
Nloci<- length(stats$trees)
t1<- read.tree(text=stats$trees[[1]][1])
plot(t1)
axisPhylo()
Age1 <- max(nodeHeights(t1))
t2<- read.tree(text=stats$trees[[2]][1])
plot(t2)
axisPhylo()
par(mfrow=c(1,2))
plot(t1)
axisPhylo()
plot(t2)
axisPhylo()
compare.chronograms(t1, t2)
t1_1 <- read.tree(text=stats$trees [[1]][1])
t1_2 <- read.tree(text=stats$trees [[1]][2])
compare.chronograms(t1_1, t1_2)
for (locus in 1:Nloci) {
	ntrees <- length(stats$trees[[locus]])
	for (n in 1:ntrees) {
		if (locus == 1 && n == 1) {
			outPhy <- read.tree(text=stats$trees[[locus]][n])
			}
			else {
				outPhy <- ape:::c.phylo(outPhy, read.tree(text=stats$trees[[locus]][n]))
				}
			}
		}		
par(mfrow=c(1,1))
densityTree(outPhy)
model3 <- coal_model(10, 50) +
feat_mutation(par_prior("theta", sample.int(100, 1))) +
sumstat_nucleotide_div()
stats <- simulate(model3, nsim = 40)
mean_pi <- sapply(stats, function(x) mean(x$pi))
theta<- sapply(stats, function(x) x$pars [["theta"]])


#questions
#No they are not all the same.
#because they calculate beteen 2 individuals.
#no it is not the same age.
#No they do not match.


#1
coalescent.plot(n=5, ngen=30, col.order="alternating")
test <-coalescent.plot()
print(test)
plot(test)
#so, the 
#2
coalescent.plot(n=12, ngen=30, col.order="alternating")
test <-coalescent.plot()
print(test)
plot(test)
#3 
coalescent.plot(n=10, ngen=30, col.order="alternating")
test <-coalescent.plot()
print(test)
plot(test)

#question 1: The first simulation started with 5.  The next started with 12.  The last started with 10. I modified these by changing the n= number in each. 

#1 
coalescent.plot(n=5, ngen=35, col.order="alternating")
test <-coalescent.plot()
print(test)
plot(test) 

#2
coalescent.plot(n=5, ngen=45, col.order="alternating")
test <-coalescent.plot()
print(test)
plot(test)

#3
coalescent.plot(n=5, ngen=50, col.order="alternating")

#4
coalescent.plot(n=5, ngen=49, col.order="alternating")
#question number 2: 49 goes to fixation at 10 
#question number 3: 5 offspring 
coalescent.plot(n=5, ngen=5, col.order="alternating")
#question 4: it has a large impact because each simulation was different each time 
#question 5: I think it is alive in generation 0
#~/Desktop/e1.pdf
#~/Desktop/e2.pdf
#~/Desktop/e3.pdf
#~/Desktop/e4.pdf
#~/Desktop/e5.pdf




setwd("~/Desktop/Evolution/Tasks/Projects")
mydata <- read.csv ("karell_data.csv")
# My hypothesis is that the telomere length will be shorter in tawny owls that are infected with the Leucocytozoon parasites. 
 x <- read.table("~/Downloads/Karell_et_al_Data_TL_and_parasite.txt", sep="\t")
head(x)
 x <- read.table("~/Downloads/Karell_et_al_Data_TL_and_parasite.txt", sep="\t")
 y <- x[-1,]
 colnames(y) <- x[1,]
 getwd()
setwd("/Users/tylerostling/Desktop/Evolution/Tasks")
 write.csv(y, "karell_data.csv")

setwd("~/Desktop/Evolution/Tasks/task_09")
library('phytools')
trees <- list()
births <- c()
Fractions <- c()
for(i in 1:100) {
	births[i] <- runif(1)
	Fractions[i] <- runif(1)
	trees[[i]] <- pbtree(b = births[i], d = (births[i] * Fractions[i]), n = 100, nsim = 1)
}
trees
trees[[i]]
plot(trees[[i]])
install.packages('geiger')
library('geiger')
#4
install.packages('TreeTools')
yes
library('TreeTools')
Y
tips <- sapply(trees, NTip)
logtips <- log(tips)
diversification <- sapply(trees, bd.ms)
plot(diversification, logtips, xlab='net diversification', ylab='log of total number of tips')
abline(lm(diversification~logtips), col='red')
#It shows a positive correlation between diversification and number of tips 
cor(diversification, logtips)
#positive correlation
#QUESTION 5
speciation <- sapply(trees, bd.km)
#for (t in 1:length(trees))	{
i <- 1
numtips <- c()
avgBL <- c()
for ( i in 1:length(trees))	{
	# choose tree
	y <- trees[[i]]
	# find the number of tips
	numtips[i] <- Ntip(y)
	# find the average branch length
	avgBL[i] <- mean(y$edge.length)
}
plot(speciation, avgBL, xlab='speciation rate', ylab='average branch length')
#branch length is oppositely proportional to speciation rate 
#6
cor(speciation, avgBL)
#correlation = -0.25
#Question 7
which.max(tips)
bigTree <- trees[[66]]
plot(bigTree)
rates <- c()
traits <- list()
for (i in 1:100) {
	rates[i] <- runif(1)
	traits[[i]] <- fastBM(tree = bigTree, sig2 = rates[i])
}
#? 8
avgtrait <- sapply(traits, mean)
avgtrait
avgrate <- sapply(rates, mean)
avgrate
correlation <- cor(avgtrait, avgrate)
print(correlation)
plot(avgrate, avgtrait)
abline(lm(avgrate~avgtrait), col='purple')
#0.09 correlation 
#9
vartraits <- sapply(traits, var)
cor(vartraits, rates)
# positive correlation between variance of traits and rates.
#? 10
trait1 <- traits[1]
trait1
trait2 <- traits[2]
trait2
traitmat <- cbind(traits[[1]], traits[[2]])
traitmat
var(traitmat)
cor(traitmat[,1], traitmat[,2])
#Correlation is about 0 and this tells us that it is not significant
plot(traitmat[,1], traitmat[,2])
abline(lm(traitmat[,1]~traitmat[,2]), col='pink')

setwd("~/Desktop/Evolution/Projects")
 siskin <- read.csv("siskin.csv")
 boxplot(siskin$X..parasitemia~siskin$dayspostinfection, boxwex=0.25, xlab="parasitemia", ylab="dpi")