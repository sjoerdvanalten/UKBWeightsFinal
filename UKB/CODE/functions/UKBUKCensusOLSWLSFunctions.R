
UKBUKCensus<-function(formula,UKC,UKB,odds=FALSE,CName="UK Census",BName="UKB",pMethod="",SEs=FALSE){
  if (odds==TRUE){
    OLSC<-glm(formula,data=UKC,weights=popweight, family=binomial(link="logit"))
    OLSUKB<-glm(formula,data=UKB,  family=binomial(link="logit"))
  }else{
    OLSC<-lm(formula,data=UKC,weights=popweight)
    OLSUKB<-lm(formula,data=UKB)
  }
  pointC<-OLSC$coefficients[2]
  pointB<-OLSUKB$coefficients[2]
  #WLSUKB<-lm(formula,weights=Weights,data=UKB)
  vcovC<-vcovHC(OLSC, type = "HC0")
  vcovB<-vcovHC(OLSUKB, type = "HC0")
  vcovCNonRobust<-coeftest(OLSC)[2,2]
  vcovBNonRobust<-coeftest(OLSUKB)[2,2]
  varEpsilonC<-var(OLSC$residuals)
  varEpsilonB<-var(OLSUKB$residuals)
  #return estimates using heteroskedasticity robust errors. (HC0 calls White's sandwhich estimator for Heteroskedasticity-robust var.)
  OLSC<-coefci(OLSC, vcov = vcovC, level=0.95)
  OLSUKB<-coefci(OLSUKB, vcov = vcovB, level=0.95)

  #WLSUKB<-coeftest(WLSUKB, vcov = vcovHC(WLSUKB, type = "HC0"))
  if (odds==TRUE){
    pointC<-exp(pointC)
    pointB<-exp(pointB)
    OLSC<-exp(OLSC)
    OLSUKB<-exp(OLSUKB)
  }
  if (pMethod=="zTest"){
    Z<-(pointC-pointB)/(sqrt((vcovC[2,2])+(vcovB[2,2])))
    print(Z)
    pVal<-1-pchisq(Z^2,1)
  }else{pVal<-NA}
  if(SEs==FALSE){
  OutC <- c(as.character(Reduce(paste,deparse(formula))),CName,pointC,OLSC[2,],pVal)
  OutB <- c(as.character(Reduce(paste,deparse(formula))),BName,pointB,OLSUKB[2,],pVal)
  Out <- rbind(OutC,OutB)
  colnames(Out)<-c('formula','data','point','CILow','CIHigh','P')}else{
  OutC <- c(as.character(Reduce(paste,deparse(formula))),CName,pointC,OLSC[2,],sqrt(vcovC[2,2]),vcovCNonRobust,varEpsilonC,pVal)
  OutB <- c(as.character(Reduce(paste,deparse(formula))),BName,pointB,OLSUKB[2,],sqrt(vcovB[2,2]),vcovBNonRobust,varEpsilonB,pVal)
  Out <- rbind(OutC,OutB)
  colnames(Out)<-c('formula','data','point','CILow','CIHigh','SE','SENonRobust','varEpsilon','P')}
  return(Out)
}


UKBWLS<-function(formula,UKBData,w,lab=NULL,odds=FALSE,SEs=FALSE){
  UKBData$tempweight<-w
  if (odds==TRUE){
    WLSB<-glm(formula,data=UKBData,weights=tempweight,family=binomial(link="logit"))
  }else{
    WLSB<-lm(formula,data=UKBData,weights=tempweight)
  }
  pointB<-WLSB$coefficients[2]
  #WLSUKB<-lm(formula,weights=Weights,data=UKB)
  #return estimates using heteroskedasticity robust errors. (HC0 calls White's sandwhich estimator for Heteroskedasticity-robust var.)
  WLSBSE<-coeftest(WLSB, vcov = vcovHC(WLSB, type = "HC0"))[2,2]
  WLSBSENonRobust<-coeftest(WLSB)[2,2]
  varEpsilonWLS<-var(WLSB$residuals)
  WLSB<-coefci(WLSB, vcov = vcovHC(WLSB, type = "HC0"), level=0.99)
  
  #WLSUKB<-coeftest(WLSUKB, vcov = vcovHC(WLSUKB, type = "HC0"))
  
  if (is.null(lab)){
    lab<-deparse(substitute(w))
  }
  if (odds==TRUE){
    pointB<-exp(pointB)
    WLSB<-exp(WLSB)
  }
  if(SEs==FALSE){
  Out <- c(as.character(Reduce(paste,deparse(formula))),lab,pointB,WLSB[2,])
  names(Out)<-c('formula','data','point','CILow','CIHigh')}else{
  Out <- c(as.character(Reduce(paste,deparse(formula))),lab,pointB,WLSB[2,],WLSBSE,WLSBSENonRobust,varEpsilonWLS)
  names(Out)<-c('formula','data','point','CILow','CIHigh','SE','SENonRobust','varEpsilon')  
  }
  return(Out)
}

UKBHeckman<-function(formula,UKBData,IMR,lab=NULL){
  UKBData$TempIMR<-IMR
  TempFormula<-as.formula(paste0(formula,"+TempIMR"))
  HeckB<-lm(TempFormula,data=UKBData)
  pointB<-HeckB$coefficients[2]
  #WLSUKB<-lm(formula,weights=Weights,data=UKB)
  #return estimates using heteroskedasticity robust errors. (HC0 calls White's sandwhich estimator for Heteroskedasticity-robust var.)
  HeckB<-coefci(HeckB, vcov = vcovHC(HeckB, type = "HC0"))
  #WLSUKB<-coeftest(WLSUKB, vcov = vcovHC(WLSUKB, type = "HC0"))
  
  if (is.null(lab)){
    lab<-deparse(substitute(w))
  }
  Out <- c(as.character(Reduce(paste,deparse(as.formula(formula)))),lab,pointB,HeckB[2,])
  names(Out)<-c('formula','data','point','CILow','CIHigh')
  return(Out)
}

PClean<-function(Pvec){
  PClean<-Pvec
  PClean[Pvec>=0.0000001]<-paste("=",formatC(Pvec[Pvec>=0.00000001],digits=3))
  #PClean[Pvec<0.0001]<-formatC(Pvec[Pvec<0.001],format="e",digits=0)
  PClean[Pvec<0.00001]<-"< 1e-08"
  PClean<-as.character(PClean)
  return(PClean)
}

