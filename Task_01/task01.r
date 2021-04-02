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




setwd("~/Desktop/Evolution/Tasks/task_08")
install.packages("phytools")
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
for(i in1:nrow(fossilData))
i<- 1 if (i== 1) {
print(Ancestors)   }
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
#question 1: Shark
#question 2: no there are not any branch lengths
#question 3: in the tips of tree, or "x" (svl); CI95 can compute 95% confidence intervals on ancestral state estimates
#question 4:
#question 7:
#question 8-10:










text.string<-

"(((((((cow, pig) , whale) , (bat, (lemur, human))) , (robin, iguana)) , coelacanth) , (gold_fish, trout)) , shark) ;"
vert.tree<-read.tree(text=text.string)
plot(vert.tree , edge.width=2)
nodelabels(frame="circle", bg='white', cex=1)
#Question 1: Shark
vert.tree
#question 2: no there are not any branch lengths 
str(vert.tree)
tree<-read.tree(text=" (((A, B) , (C, D)), E) ;")
plotTree(tree, offset=1)
tiplabels(frame="circle", bg='lightblue', cex=1)
nodelabels(frame="circle", bg='white' , cex=1)
tree$tip.label
tree$edge
AnolisTree<-force.ultrametric(read.tree ("https://jonsmitchell.com/data/anolis.tre"))
par(las=1)
hist(AnolisTree$edge.length, col='black', border='white', main="", xlab="edge lengths for the Anolis tree", ylim=c(0, 50), xlim=c(0, 6))
tipEdges<- which(AnolisTree$edge[ , 2] <= Ntip(AnolisTree))
Lengths <- AnolisTree$edge.length
names(Lengths) <- AnolisTree$tip.label
names(Lengths) [which(Lengths==min (Lengths))]
plot(AnolisTree, cex=0.25)
Labs<-sapply(AnolisTree$edge.length, round, digits=2)
edgelabels(text=Labs, cex=0.25)
?plot.phylo
plotTree(AnolisTree, offset=1, show.tip.label=FALSE)
#circular_tree(AnolisTree)
plotTree(AnolisTree, offset=1, show.tip.label=TRUE, tip.color="red")
#6,7,8 
AnolisTree$edge
EdgesThatAreTips <-which(AnolisTree$edge[, 2] <= Ntip(AnolisTree))
TipLengths <- AnolisTree$edge.length[EdgesThatAreTips]
AnolisTree$edge.length
AnolisShortestTips <-which(TipLengths == min (TipLengths))
Anolistree_no_short <- drop.tip(tree, AnolisShortestTips[1])
plot(Anolistree_no_short)
ltt(AnolisTree)
abline(0, 1, lwd=2, col='red', lty=2)


setwd("~/Desktop/Evolution/Tasks/task_08")
library("phytools")
tree <- read.tree("https://jonsmitchell.com/data/anolis.tre")
plot(tree,type="fan")
tree$tip.label
#question 1: 1:82 tips
data <- read.csv("https://jonsmitchell.com/data/svl.csv", stringsAsFactor=F, row.names=1)
data
data[, 1]
# question 2: 100 demensions; lst of species
svl <- setNames(data$svl, rownames(data))
svl
Ancestors <- fastAnc(tree, svl, vars=TRUE, Cl=TRUE)
#queston 3:
#question 4:

par(mar=c(0.1, 0.1, 0.1, 0.1))
plot(tree, type="fan", lwd=2, show.tip.label=F)
tiplabels(pch=16, cex=0.25*svl[tree$tip.label])
nodelabels(pch=16, cex=0.25*Ancestors$ace)


setwd("~/Desktop/Evolution/Tasks/task_08")
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
 # adds the ancestral states 
 nodelabels(pch=16, cex=0.25*Ancestors$ace)
 obj <- contMap(tree, svl, plot=F)
 plot (obj, type="fan", legend= 0.7*max(nodeHeights(tree)), sig=2, fsize=c(0.7, 0.9))
 fossilData <- data.frame(svl=log(c(25.4, 23.2, 17.7, 19.7, 24, 31)), tip1=c("Anolis_aliniger", "Anolis_aliniger", "Anolis_occultus", "Anolis_ricordii", "Anolis_cristatellus", "Anolis_occultus"), tip2=c("Anolis_chlorocyanus", "Anolis_coelestinus", "Anolis_hendersoni", "Anolis_cybotes", "Anolis_angusticeps", "Anolis_angusticeps"))
 fossilNodes <-c()
 nodeN <-c()
{
	for(i in 1:nrow(fossilData))
	i<- 1 if (i == 1) {
print(Ancestors)   }
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

