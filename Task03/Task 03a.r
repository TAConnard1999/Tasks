trueMean1 <− 5
trueSD1 <− 5
population1 <− rnorm(1e6 ,trueMean1 ,trueSD1)
trueMean2 <− 4
trueSD2 <− 5
population2 <− rnorm(1e6 ,trueMean2 ,trueSD2)
Size<− 50
Sample1 <− sample(population1,Size)
Sample2 <− sample(population2,Size)
 ### Yes the samples and populations are both different. 
boxplot ( Sample1 , Sample2 )
source ("http://jonsmitchell.com/code/simFxn04.R")
MatGrandma <− makeFounder ("grandma_mom")
MatGrandpa <− makeFounder ("grandpa_mom")
PatGrandma <− makeFounder ("grandma_da")
PatGrandpa <− makeFounder ("grandpa_da")
Alan<−makeBaby(PatGrandma,PatGrandpa)
Brenda<-makeBaby(MatGrandma,MatGrandpa)
Focus<−makeBaby(Brenda,Alan)
ToMom<−length(grep("mom",Focus))/length(Focus)
ToMomMom<−length(grep("grandma_mom",Focus))/length(Focus)
ToMomDad<−length(grep("grandpa_mom",Focus))/length(Focus)
ToDadMom<-length(grep("grandma_da",Focus))/length(Focus)
ToDadDad<-length(grep("grandpa_da",Focus))/length(Focus)
ToDad<-length(grep("dad",Focus))/length(Focus)
 ### No Focus is not equally related to the maternal grandparents nor is he equally related to his paternal grandparents. This is what I expected. His average relatedness to all four grandparents is 0.5
Sibling01<−makeBaby(Brenda,Alan)
 ### I expect Focus and Sibling_01 to share at least 50% of the same DNA.
ToSib<−length(intersect(Focus,Sibling01))/length(Focus)
 ### After 1000 reproductions I would assume that the relatedness would fall somewhere near 50%
ManySiblings<−replicate(1e3,length(intersect(Focus,makeBaby(Brenda,Alan)))/length(Focus))
quantile(ManySiblings)
mean(ManySiblings)
plot(density(ManySiblings),main="",xlab="proportion shared genes")
 ### The range of values shows a normal distribution of gene proportion vs population density.
HWE<−function(p){
	aa<-p^2
	ab<-2*p*(1-p)
	bb<-(1-p)^2
	return(c(aa=aa,ab=ab,bb=bb))
}
plot(1,1,type="n",xlim=c(0,1),ylim=c(0,1),xlab="freq.allelea",ylab="geno.freq")
p<−seq(from=0,to=1,by=0.01)
GenoFreq<−t(sapply(p,HWE))
lines(p,GenoFreq[,"aa"],lwd=2,col="red")
 ### Yes I think I can understand this plot. The frequency of aa genotype people increase as the allele a increases, and as allele a decreases the genotype decreases. Time is not shown for this plot, not is geographic space. 
lines(p,GenoFreq[,"ab"],lwd=2,col="purple")
lines (p,GenoFreq[,"bb"],lwd=2,col="blue")
legend("top",legend=c("aa","ab","bb"),col=c("red","purple","blue"),lty=1,lwd=2,bty="n")
Pop<−simPop(500)
points(Pop[,"freqa"],Pop[,"Genotypes.aa"]/500,pch=21,bg="red")
 ### Yes the points plotted from the simulation pretty tightly follow the expectations of Hardy-Weinberg.
Pop<−simPop(50)
points(Pop[,"freqa"],Pop[,"Genotypes.aa"]/50,pch=22,bg="red")
 ### The points from the lower population became less tightly knit to the Hardy Weinberg prediction, but that's to be expected because a lower population is going to have a higher standard deviation.
install.packages("learnPopGen")
library(learnPopGen)
x<−genetic.drift(Ne=200,nrep=5,pause=0.01)
x<−genetic.drift(Ne=100,nrep=5,pause=0.01)
x<−genetic.drift(Ne=50,nrep=5,pause=0.01)
x<−genetic.drift(Ne=1000,nrep=5,pause=0.01)
PopSizes<−5:50
Samples<−rep(PopSizes,5)
tExt<−sapply(Samples,function(x)nrow(simPop(x,500)))
Line<-lm(tExt~Samples)
summary(Line)
Line$coef
plot(Samples,tExt)
abline(Line)
Line2<-lm(tExt~Samples+0)
summary(Line2)
Line2$coef
abline(Line2)
 ### As the population size increases the points become further apart. To me this means that as population size increases less genes become fixed to the expected value or the line. This is because as population size increases genetic drift is less inluential. 

