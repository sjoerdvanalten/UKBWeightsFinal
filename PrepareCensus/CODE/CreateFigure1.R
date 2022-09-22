#Goal: Simulate some data to illustrate the effects of collider bias in the UKB:
library(tidyverse)
library(ggExtra)
library(survey)
library(lmtest)
library(sandwich)
library(estimatr)
library(svMisc)
library(extrafont)
font_import()
loadfonts(device = "win")

set.seed(3454353)

X<-rnorm(50000, mean =0, sd= 1)

Error<-rnorm(50000, mean = 0, sd = 1)

#Scenario 1, x and y positively related, selection on y positive:
Y<-X+rnorm(50000, mean =0, sd =1)
Data<-data.frame(X,Y,Error)
Data$UKB <- Data$Y
Data$UKB <- as.factor(Data$UKB > quantile(Data$UKB, .95))

FillColors<-c("#E69F00", "#56B4E9")

png("../OUTPUT/FIGURES/SelectionBiasScenario1a.png")
ggplot(data=Data,aes(x = X, y = Y, color = UKB)) + geom_point(show.legend=FALSE, alpha=0.3) +
  geom_smooth(data=Data,method="lm",colour = "#E69F00",size=1.3) +
  geom_smooth(data=Data[Data$UKB==TRUE,],method="lm",colour = "#56B4E9",size=1.3) +
  xlim(-5,5) + ylim(-5,5) +#ggtitle("P(UKB)~Y") +
  scale_color_manual(values=FillColors)+theme(panel.grid.major = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"),
                                              axis.title=element_text(size=35),axis.text.y=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank(),axis.ticks.y=element_blank())
dev.off()

summary(lm(Y~X,data=Data))
summary(lm(Y~X,data=Data[Data$UKB==TRUE,]))
sd(Data$X)
sd(Data$X[Data$UKB==TRUE])
sd(Data$Y)
sd(Data$Y[Data$UKB==TRUE])
#Scenario 1b, x and Y positively related, selection on x positive:
Data<-data.frame(X,Y,Error)
Data$UKB <- Data$X
Data$UKB <- as.factor(Data$UKB > quantile(Data$UKB, .95))

FillColors<-c("#E69F00", "#56B4E9")
png("../OUTPUT/FIGURES/SelectionBiasScenario1b.png")
ggplot(data=Data,aes(x = X, y = Y, color = UKB)) + geom_point(show.legend=FALSE, alpha=0.3) +
  geom_smooth(data=Data,method="lm",colour = "#E69F00",size=1.3,fullrange=TRUE) +
  geom_smooth(data=Data[Data$UKB==TRUE,],method="lm",colour = "#56B4E9",size=1.3,fullrange=TRUE) +
  xlim(-5,5) + ylim(-5,5) +#ggtitle("P(UKB)~X") +
  scale_color_manual(values=FillColors)+theme(panel.grid.major = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"),
                                              axis.title=element_text(size=35),axis.text.y=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank(),axis.ticks.y=element_blank())
dev.off()

summary(lm(Y~X,data=Data))
summary(lm(Y~X,data=Data[Data$UKB==TRUE,]))
sd(Data$X)
sd(Data$X[Data$UKB==TRUE])
sd(Data$Y)
sd(Data$Y[Data$UKB==TRUE])

#Scenario 1c, x and y positivelY related, selection on x and y positive:
Data<-data.frame(X,Y,Error)
Data$UKB <- 0.5*Data$Y+0.5*Data$X
Data$UKB <- as.factor(Data$UKB > quantile(Data$UKB, .95))

FillColors<-c("#E69F00", "#56B4E9")
png("../OUTPUT/FIGURES/SelectionBiasScenario1c.png")
ggplot(data=Data,aes(x = X, y = Y, color = UKB)) + geom_point(show.legend=FALSE, alpha=0.3) +
  geom_smooth(data=Data,method="lm",colour = "#E69F00",size=1.3,fullrange=TRUE) +
  geom_smooth(data=Data[Data$UKB==TRUE,],method="lm",colour = "#56B4E9",size=1.3,fullrange=TRUE) +
  xlim(-5,5) + ylim(-5,5) +#ggtitle("P(UKB)~0.5*Y+0.5*X") +
  scale_color_manual(values=FillColors)+theme(panel.grid.major = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"),
                                              axis.title=element_text(size=35),axis.text.y=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank(),axis.ticks.y=element_blank())
dev.off()

summary(lm(Y~X,data=Data))
summary(lm(Y~X,data=Data[Data$UKB==TRUE,]))
sd(Data$X)
sd(Data$X[Data$UKB==TRUE])
sd(Data$Y)
sd(Data$Y[Data$UKB==TRUE])

#Scenario 1d, x and Y positivelY related, selection on x and Y negative:
Data<-data.frame(X,Y,Error)
Data$UKB <- Data$Y-2*Data$X
Data$UKB <- as.factor(Data$UKB > quantile(Data$UKB, .95))

FillColors<-c("#E69F00", "#56B4E9")
png("../OUTPUT/FIGURES/SelectionBiasScenario1d.png")
ggplot(data=Data,aes(x = X, y = Y, color = UKB)) + geom_point(show.legend=FALSE, alpha=0.3) +
  geom_smooth(data=Data,method="lm",colour = "#E69F00",size=1.3,fullrange=TRUE) +
  xlim(-5,5) + ylim(-5,5) +
  geom_smooth(data=Data[Data$UKB==TRUE,],method="lm",colour = "#56B4E9",size=1.3,fullrange=TRUE) +#ggtitle("P(UKB)~0.5*Y-2*X") +
  scale_color_manual(values=FillColors)+theme(panel.grid.major = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"),
                                              axis.title=element_text(size=35),axis.text.y=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank(),axis.ticks.y=element_blank())
dev.off()

summary(lm(Y~X,data=Data))
summary(lm(Y~X,data=Data[Data$UKB==TRUE,]))
sd(Data$X)
sd(Data$X[Data$UKB==TRUE])
sd(Data$Y)
sd(Data$Y[Data$UKB==TRUE])

#Scenario 2, x and y unrelated, selection on y positive:
Y<-rnorm(50000, mean =0, sd =1)
Data<-data.frame(X,Y,Error)
Data$UKB <- Data$Y
Data$UKB <- as.factor(Data$UKB > quantile(Data$UKB, .95))

FillColors<-c("#E69F00", "#56B4E9")

png("../OUTPUT/FIGURES/SelectionBiasScenario2a.png")
ggplot(data=Data,aes(x = X, y = Y, color = UKB)) + geom_point(show.legend=FALSE, alpha=0.3) +
  geom_smooth(data=Data,method="lm",colour = "#E69F00",size=1.3) +
  geom_smooth(data=Data[Data$UKB==TRUE,],method="lm",colour = "#56B4E9",size=1.3) +
  xlim(-5,5) + ylim(-5,5) +#ggtitle("P(UKB)~Y") +
  scale_color_manual(values=FillColors)+theme(panel.grid.major = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"),
                                              axis.title=element_text(size=35),axis.text.y=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank(),axis.ticks.y=element_blank())
dev.off()

#Scenario 2b, x and Y unrelated, selection on x positive:
Data<-data.frame(X,Y,Error)
Data$UKB <- Data$X
Data$UKB <- as.factor(Data$UKB > quantile(Data$UKB, .95))

FillColors<-c("#E69F00", "#56B4E9")
png("../OUTPUT/FIGURES/SelectionBiasScenario2b.png")
ggplot(data=Data,aes(x = X, y = Y, color = UKB)) + geom_point(show.legend=FALSE, alpha=0.3) +
  geom_smooth(data=Data,method="lm",colour = "#E69F00",size=1.3,fullrange=TRUE) +
  geom_smooth(data=Data[Data$UKB==TRUE,],method="lm",colour = "#56B4E9",size=1.3,fullrange=TRUE) +
  xlim(-5,5) + ylim(-5,5) +#ggtitle("P(UKB)~X") +
  scale_color_manual(values=FillColors)+theme(panel.grid.major = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"),
                                              axis.title=element_text(size=35),axis.text.y=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank(),axis.ticks.y=element_blank())
dev.off()


#Scenario 2c, x and y unrelated, selection on x and y positive:
Data<-data.frame(X,Y,Error)
Data$UKB <- 0.5*Data$Y+0.5*Data$X
Data$UKB <- as.factor(Data$UKB > quantile(Data$UKB, .95))

FillColors<-c("#E69F00", "#56B4E9")
png("../OUTPUT/FIGURES/SelectionBiasScenario2c.png")
ggplot(data=Data,aes(x = X, y = Y, color = UKB)) + geom_point(show.legend=FALSE, alpha=0.3) +
  geom_smooth(data=Data,method="lm",colour = "#E69F00",size=1.3,fullrange=TRUE) +
  geom_smooth(data=Data[Data$UKB==TRUE,],method="lm",colour = "#56B4E9",size=1.3,fullrange=TRUE) +
  xlim(-5,5) + ylim(-5,5) + #ggtitle("P(UKB)~0.5*Y+0.5*X") +
  scale_color_manual(values=FillColors)+theme(panel.grid.major = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"),
                                              axis.title=element_text(size=35),axis.text.y=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank(),axis.ticks.y=element_blank())
dev.off()


#Scenario 2d, x and Y positivelY related, selection on x and Y negative:
Data<-data.frame(X,Y,Error)
Data$UKB <- Data$Y-2*Data$X
Data$UKB <- as.factor(Data$UKB > quantile(Data$UKB, .95))

FillColors<-c("#E69F00", "#56B4E9")
png("../OUTPUT/FIGURES/SelectionBiasScenario2d.png",width=1920,height=1920,res=288)
ggplot(data=Data,aes(x = X, y = Y, color = UKB)) + geom_point(show.legend=TRUE) +
  scale_color_manual(values=FillColors,labels=c("Not in S","In S"))+
  geom_smooth(data=Data,method="lm",aes(alpha="Regression line, full population",colour = "#E69F00"),colour = "#E69F00",size=1.3,fullrange=TRUE) +
  geom_smooth(data=Data[Data$UKB==TRUE,],aes(alpha="Regression line, performed in S only",colour = "#56B4E9"),method="lm", colour = "#56B4E9",size=1.3,fullrange=TRUE,
              ) +
  scale_alpha_manual(name = NULL,
                     values = c(1, 1),
                     breaks = c("Regression line, full population", "Regression line, performed in S only"),guide = guide_legend(override.aes = list(color = FillColors) ) )+#ggtitle("P(UKB)~0.5*Y-2*X") +
  xlim(-5,5) + ylim(-5,5) +
  theme(panel.grid.major = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"),
                                              axis.title=element_text(size=35),text=element_text(family="TT Times New Roman"),axis.text.y=element_blank(),
        axis.text.x=element_blank(),axis.ticks.x=element_blank(),axis.ticks.y=element_blank())
dev.off()
