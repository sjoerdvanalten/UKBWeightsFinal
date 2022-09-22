library(tidyverse)

options(bitmapType="cairo")

Weights<-read.table(paste0("../TEMP/UKBLassoWeights",1,".csv"),header=TRUE,sep=",")
Weights$Test<-1
for (K in c(2:5)){
  temp<-read.table(paste0("../TEMP/UKBLassoWeights",K,".csv"),header=TRUE,sep=",")
  temp$Test<-K
  Weights<-rbind(Weights,temp)
}



summary(Weights$LassoWeight)
sd(Weights$LassoWeight)
p1<-quantile(Weights$LassoWeight,0.01)
p99<-quantile(Weights$LassoWeight,0.99)

print(paste("weights are cut off at",p1,"&",p99))
Weights$LassoWeight[Weights$LassoWeight<p1]<-p1
Weights$LassoWeight[Weights$LassoWeight>p99]<-p99


summary(Weights$LassoWeight)
sd(Weights$LassoWeight)

table(K)

png("../OUTPUT/FIGURES/WeightHistoFullWinsor.png",width = 1080, height = 1080, res=288)
ggplot(Weights, aes(LassoWeight)) + geom_histogram(fill="lightblue",binwidth=0.01) + theme(axis.line = element_line(colour = "black"),
                                                                                    panel.background = element_blank(),
                                                                                    axis.text=element_text(size=12),
                                                                                    axis.title=element_text(size=14,face="bold")) +
  xlab("UKB Selection weight")
dev.off()

#TestAssign <- read.table("../TEMP/TestAssign.csv",header=TRUE,sep=",") 

summary(Weights$LassoWeight)

#Weights<-merge(Weights,TestAssign)

write.table(Weights,"../OUTPUT/UKBWeightsKFolded.csv",row.names=FALSE,quote=FALSE,sep=",")
#Assign each UKB respondent a weight base don their training sample: 
