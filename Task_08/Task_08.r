setwd("C:\\users\\Tris\\Desktop\\Evolution\\Tasks\\Task_08")
library("phytools")
library("ape")
library("maps")
tree <- read.tree('https://jonsmitchell.com/data/anolis.tre')
plot(tree, type="fan")
	### Question 1: There are 82 tips and yes there are branch lengths.
data <- read.csv('https://jonsmitchell.com/data/svl.csv', stringsAsFactors=F, row.names=1)
	### Question 2: In this case data is a list that represents 82 different species.
class(data)
str(data)
typeof(data)
svl <- setNames(data$svl, rownames(data))
Ancestors <- fastAnc(tree, svl, vars=TRUE, CI=TRUE)
?fastAnc
	### Question 3: The values stores are the estimates of ancestry and the CI95 element stands for a 95% confidence interval. 
	###	Question 4:
par(mar=c(0.1,0.1,0.1,0.1))
plot(tree, type='fan', lwd=2, show.tip.label=F)
tiplabels(pch=16, cex=0.25*svl[tree$tip.label])
nodelabels(pch=16, cex=0.25*Ancestors$ace)
obj <- contMap(tree, svl, plot=F)
plot(obj, type="fan", legend=0.7*max(nodeHeights(tree)), sig=2, fsize=c(0.7, 0.9))
fossilData <- data.frame(svl=log(c(25.4, 23.2, 17.7, 19.7, 24, 31)), tip1=c("Anolis_aliniger", 'Anolis_aliniger', 'Anolis_occultus', 'Anolis_ricordii', 'Anolis_cristatellus', 'Anolis_occultus' ), tip2=c('Anolis_chlorocyanus', 'Anolis_coelestinus', 'Anolis_hendersoni', 'Anolis_cybotes', 'Anolis_angusticeps', 'Anolis_angusticeps'))
	### Question 5: 
{	
	for(i in 1:nrow(fossilData))
	i <- 1
	if( i == 1) {
	print(Ancestors)
	}
}
fossilNodes <- c()
nodeN <- c()
Node <- fastMRCA(tree, fossilData[i, "tip1"], fossilData[i, "tip2"])
Node
fossilNodes[i] <- fossilData[i, "svl"]
fossilNodes[i]
nodeN[i] <- Node
names(fossilNodes) <- nodeN
Ancestors_withFossils <- fastAnc(tree, svl, anc.states=fossilNodes, CI=TRUE, var=TRUE)
Ancestors_withFossils
Ancestors_withoutFossils <- fastAnc(tree, svl, CI=TRUE, var=TRUE)
Ancestors_withoutFossils
Ancestors_withFossils$ace
Ancestors_withoutFossils$ace
plot(Ancestors_withFossils$ace, Ancestors_withoutFossils$ace,xlab="with fossils",ylab="without fossils", pch=12, cex=1, col="orange")
	###	Question 7: The larger the ancestral size the more fossil data.
	###	Question 8-10: AIC: BM= -6.512, OU= -4.517, EB= -7.235, kappa= -4.512, delta= -6.107, rate_trend= -6.981, white= 91.391
install.packages("geiger")
library("geiger")
?fitContinuous
fitContinuous(tree, svl, model="BM")
fitContinuous(tree, svl, model="OU")
fitContinuous(tree, svl, model="EB") 
fitContinuous(tree, svl, model="kappa")
fitContinuous(tree, svl, model="delta")
fitContinuous(tree, svl, model="rate_trend")
fitContinuous(tree, svl, model="white")
	### The lowest AIC value best fits the data which means that EB is the best model in this instance. 
	