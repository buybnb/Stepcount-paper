library(data.table);library(reshape);library(dplyr);library(lmerTest)
library(dlnm);library(splines);library(tsModel)
library(ggplot2)
library(ggpubr)

##Import Data

data <- fread("F:\\Current Work\\Physical and temp\\steps.csv",stringsAsFactors = F,header = T)


####Modeling individual DLNM - Temperature
tm = data[,c("temp","templag1","templag2","templag3","templag4","templag5","templag6")]
cb1 <- crossbasis(tm,lag=6,argvar=list(fun="bs",df=4),arglag=list(fun="bs",df=4))
model1<- lmer(step ~ cb1 +O3+PM2.5+age+sex +(1|id),data=data)

####RH
rhh=data[,c("rh","rhlag1","rhlag2","rhlag3")]
cb2 <- crossbasis(rhh,lag=3,argvar=list(fun="bs",df=4),arglag=list(fun="bs",df=4))
model2<- lmer(step ~ cb2 +O3+PM2.5+age+sex +(1|id),data=data)


####Wind Speed
cb3=onebasis(data$ws,"bs",df=4)
model3<- lmer(step ~ cb3 +O3+PM2.5+age+sex +(1|id),data=data)

###Rainfall
cb4=onebasis(data$rain,"bs",df=4)
model4<- lmer(step ~ cb4 +O3+PM2.5+age+sex +(1|id),data=data)



###Modeling QG-DLNM
percentile=function(data,Xnm){
  for(i in 1:length(Xnm)){
    k=(range(data[,Xnm[i]])[2]-range(data[,Xnm[i]])[1])/100
    kk=seq(range(data[,Xnm[i]])[1],range(data[,Xnm[i]])[2],k)
    data[,Xnm[i]] =(cut(data[,Xnm[i]], breaks = kk, labels = FALSE, include.lowest = TRUE) - 1)
  }
  data
}
Xnm=c("temp","templag1","templag2","templag3","templag4","templag5","templag6","rh","rhlag1","rhlag2","rhlag3","ws","rain")
data2=percentile(data,Xnm)

model7<- lmer(step ~ crossbasis(cbind(temp,templag1,templag2,templag3,templag4,templag5,templag6),
                                lag=6,argvar=list(fun="bs",df=4),arglag=list(fun="bs",df=4)) +
                crossbasis(cbind(rh,rhlag1,rhlag2,rhlag3),
                           lag=3,argvar=list(fun="bs",df=4),arglag=list(fun="bs",df=4))+
                bs(ws,df=4)+bs(rain,df=4)+age+sex +(1|id),data=data2)

dt2=NULL
for(j in 1:100){
  da=data2
  da[,Xnm]=(j-1)
  da$psi=(j-1)
  #da$y=predict(model,da,type="response")
  dt2=rbind(dt2,da)
}  

dt3=cbind(dt2,y=predict(model7,dt2,type="response"))
uu=glm(y~poly(psi,degree=2,raw=T),data=dt3,family = gaussian())




