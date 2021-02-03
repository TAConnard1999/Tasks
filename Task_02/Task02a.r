setwd('C:\\Users\\trist\\Desktop\\Evolution\\Tasks\\Task_02')
data<-read.csv('http://jonsmitchell.com/data/beren.csv', stringsAsFactors=F)
write.csv(data, file="rawdata.csv")
length(data)
nrow(data)
ncol(data)
colnames(data)
head(data)
data [1,]
data[2,]
data[1:3,]
data[1:3,4]
data[1:5,1:3]
Feeds<-which(data[,9]== 'bottle')
berenMilk<-data[Feeds,]
head(berenMilk)
nrow(berenMilk)
 ### There are 323 rows 
Feeds<-which(data[,'event'] == 'bottle')
Feeds<-which(data$event == 'bottle')
berenMilkA<−data[Feeds,]
berenMilkB<−data[Feeds,]
berenMilk==berenMilkA
berenMilk==berenMilkB
berenMilkA==berenMilkB
 ### Because all of the outcomes are TRUE that each selection = one another that leads me to believe that they are the same.
 dayID<-apply(data,1,function(x) paste(x[1:3], collapse='-'))
 dateID<-sapply(dayID, as.Date, format = "%Y-%m-%d", origin = "2019-04-18")
 data$age<-dateID - dateID [which(data$event == 'birth')]
 head(data)
 beren2<-data
beren3<-beren2[order(beren2$age),]
write.csv(beren3,'beren_new.csv', quote=F, row.names=FALSE)
 ### Had help from Timmy Daniels and Raza with parts involving testing whether the methods were the same. 