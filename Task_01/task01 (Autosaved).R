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





mydata <- read.csv ("Evolutionproject.csv")
# My hypothesis is that the telomere length will be shorter in tawny owls that are infected with the Leucocytozoon parasites. 


setwd("~/Desktop/Evolution/Tasks/task_6")
source ("http://jonsmitchell.com/code/reformatData07.R")
source("http://jonsmitchell.com/code/simFxn.R")
plot (1, 1, type="n", xlim=c (1998, 2013), ylim=c(0, 1))
s <- apply (overallFreq, 2, function(x) lines (overallFreq [,1], x, col= rgb(0, 0, 0, 0.01)))
rescaleFreq <- apply(overallFreq [,3:ncol(overallFreq)], 2, function(x) x-x[1])
plot(1, 1, type= "n", xlim=c(1998, 2013), ylim=c(-0.25, 0.25))
s <- apply(rescaleFreq , 2, function(x) lines(overallFreq[,1], x, col=rgb(0, 0, 0, 0.01)))
dYear <-c()
dAlleles <<- c()
for (i in 3:ncol(overallFreq))  {
	dYear <- c(dYear, overallFreq[,1])
	Vec <- overallFreq [,i]
	Init <- overallFreq [1,i]
	dAlleles <- c(dAlleles, Vec - Init)
	}
	smoothScatter(dYear, dAlleles, colramp= Pal, nbin=100)
smoothScatter (dYear, dAlleles, colramp= Pal, nbin=100, xlab="year", ylab="change in allele freq. since 1998")
addFit(nruns = 50, n= 100, ngens = 18, startT = 1997, simCol = "gray40", rescale = TRUE)
plot(alleleFreqs$d_freq, alleleFreqs$d_imm, xlim=c(-0.15, 0.15), xlab="overall freq. change", ylab="freq. change in subset")
points(alleleFreqs$d_freq, alleleFreqs$d_birth, col='blue')
points(alleleFreqs$d_freq, alleleFreqs$d_surv, col='red')