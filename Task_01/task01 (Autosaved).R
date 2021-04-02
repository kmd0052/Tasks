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
