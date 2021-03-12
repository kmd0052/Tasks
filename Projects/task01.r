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
