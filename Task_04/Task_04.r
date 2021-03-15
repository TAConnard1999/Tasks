source ("http://jonsmitchell.com/code/fxn05.R")
Pop1<−simPop(Popsize=50,nGenerations=100,initial_p=0.5,h=1,s=0)
plot(1:nrow(Pop1),Pop1[,1],ylim=c(0,1),type=”l”,xlab="generation",ylab="allelefreq.",lwd=2)
lines(1:nrow(Pop1),Pop1[,2],lwd=2,col='red')
legend("topleft",legend=c("a","b"),col=c("black","red"),lwd=2,bty="n")
plotFit(nruns=10,n=50,ngens=100,init_p=0.5,h=1,s=0)
Expectation<−c(10,10,10,10)
Observed<−c(15,15,5,5)
Chisq<-sum(((Expectation-Observed)^2)/Expectation)
barplot(rbind(Expectation,Observed),beside=T,main=bquote(chi^2 ~ "="~.(Chisq)),legend.text=c("expected","observed"))
Expectation1<-c(2,3,10,30)
Observed1<-c(5,0,0,35)
Chisq1<-sum(((Expectation1-Observed1)^2)/Expectation1)
Chisq1
barplot(rbind(Expectation1,Observed1),beside=T,main=bquote(chi^2~"="~.(Chisq1)),legend.text=c("expected1","observed1"))
Expectation1<-c(2,3,10,30)
Observed1<-c(10,10,10,10)
Chisq1<-sum(((Expectation1-Observed1)^2)/Expectation1)
Chisq1
barplot(rbind(Expectation1,Observed1),beside=T,main=bquote(chi^2~"="~.(Chisq1)),legend.text=c("expected1","observed1"))
Expectation1<-c(2,3,10,30)
Observed1<-c(40,0,0,0)
Chisq1<-sum(((Expectation1-Observed1)^2)/Expectation1)
Chisq1
barplot(rbind(Expectation1,Observed1),beside=T,main=bquote(chi^2~"="~.(Chisq1)),legend.text=c("expected1","observed1"))
	###When the observed is all 10s the chi value is 61.7 and when all values are in one category, the value significantly increased to 765.
	### The larger the chi^2 value becomes the less even the bars between observed and expected become.
setwd("C:\\Users\\trist\\Desktop\\Evolution\\Tasks\\Task_04")
results<-read.csv("http://jonsmitchell.com/data/biol112labresults.csv",stringsAsFactors=F)
write.csv(results,"Biol112results.csv",quote=F)
counts<-results[,c("yellow","red","green","blue","black","tan")]
backgrounds<-c("White","Red","Yellow","Green","Blue","Black")
calcChi(counts[1,])
Chisqs<-apply(counts,1,calcChi)
plotChis(counts)
	### The lower the chi-square value the more eveness is observed between the bars. When the chi-square value is high, the bars become more uneven. When the chi-square value is high the expected values and observed values drift further apart.
Avg<-mean(Chisqs)
	### The avg value is 60.99, which is far greater than the critical value of 11.70, because of this we can conclude that the difference between our observed and expected outcomes are significant. 
backgroundAvgs<-tapply(Chisqs,results[,3],mean)
	###Yes, the chi-square value differs per background.
propSig<-length(which(Chisqs>11.70))/length(Chisqs)
percSig<-round(100*propSig)
	### The percent of trials with a significant p-value was 92% which is very high and surprising to me. I don't think that natural selection alone could drive the number that high. 
par(las=1, mar = c(4,4,1,1),mgp = c(2,0.5,0),tck=-0.01,cex.axis=1)
hist(Chisqs ,main="",xlab=" chi-squared values",ylab="frequency")
par(las=1,mar=c(4,4,1,1),mgp=c(2,0.5,0),tck=-0.01,cex.axis=1)
plot(1,1,xlim=c(0,400),ylim=c(1,8.5),xlab="",ylab="",type="n",yaxt="n" )
axis(2,at=1:length(backgrounds),labels=backgrounds)
mtext(side=1,expression(chi^2),cex=1.75,line=2.5)
counter<- 1
for ( i in backgrounds) {
  Data<-Chisqs[which(results[,3] == i)]
  addHist(Y=counter,Dat=Data,Color=backgroundCol[counter])
  counter<- counter+1
}
abline(v=11.70,lty=2,lwd=2,col="black")
	### I don't notice much difference between the backgrounds by just looking at the graph. 
Simulation<−simDraws(10000)
addHist(Y=7,Dat=Simulation,Color="lightgray")
mtext(side=2,at=7,line=0,"simulated")
abline(v =11.70,lty=2,lwd=2)
	### The percentage of significant results in the selection free simulation was greater than 90%. The counts in the selection free sim was different because in the intial count we were basing our results on selection for camouflage. 
Fit<−c(1,1,1,1,1,1)
names(Fit)<−1:6
Simulation2<-simDraws(1e4,w=Fit)
addHist(Y=8,Dat=Simulation2,Color=rgb(0,0,0,0.25))
Fit<-c(0.1,1,1,1,1,1)
names(Fit)<-1:6
Simulation3<-simDraws(1e4,w=Fit)
addHist(Y=8,Dat=Simulation3,Color=rgb(0,0,0,0.25))
Fit<-c(0.5,0.6,0.7,1,1,1)
names(Fit)<-1:6
Simulation4<-simDraws(1e4,w=Fit)
addHist(Y=8,Dat=Simulation4,Color=rgb(0,0,0,0.25))
Fit<-c(0.1,0.2,0.3,0.4,0.5,1)
names(Fit)<-1:6
Simulation5<-simDraws(1e4,w=Fit)
addHist(Y=8,Dat=Simulation5,Color=rgb(0,0,0,0.25))
Fit<-c(0.1,0.1,0.1,0.1,0.1,1)
names(Fit)<-1:6
Simulation6<-simDraws(1e4,w=Fit)
addHist(Y=8,Dat=Simulation6,Color=rgb(0,0,0,0.25))
mtext(side = 2, at=8, line=0, "sel.sim.")
Simulation7<-c(Simulation2,Simulation3,Simulation4,Simulation5,Simulation6)
addHist(Y=8,Dat=Simulation7,Color=rgb(0,0,1,0.25))
	### In the lab done by students I think natural selection as well as other evolutionary mechanisms are at work, whereas in the simulation we are specifically simulating natrual selection. The graphs tell us that other evolutionary processes such as drift are relatively strong forces. Comparing the students numbers to the simulated numbers tells us more because there is more data to compare to. If you were to incorporate the possibility of mutation the chi-square value would increase. 
