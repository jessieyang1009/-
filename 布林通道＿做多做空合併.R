library(quantmod)
rm(list=ls())
STK=get(getSymbols("5269.TW"))#6533
STK=na.omit(STK)
#chartSeries(STK)
############long單股買賣
par(mfrow=c(1,3))
PL=setNames(rep(0,length(Cl(STK))),time(STK))
PZ=0
row1=Cl(STK) <= BBands(Cl(STK))[,1]
row2=Cl(STK) >= BBands(Cl(STK))[,3]
for (m in 21:nrow(STK)){
  if (PZ==0 && row1[m-1]==0 && row1[m]==1){
    long=as.numeric(Cl(STK)[m])
    PZ=1
  }
  if (PZ==1 #&& row2[m-1]==0 
      && row2[m]==1){
    PL[m]=as.numeric(Cl(STK)[m]-long)
    PZ=0  
  }
}
DD=cumsum(PL)-cummax(cumsum(PL))
yRang=range(DD,cumsum(PL))
plot(DD,type="h",col="darkgreen",ylim=yRang)
par(new=T)
plot(cumsum(PL),col="gold",lwd=2,type="h",ylim=yRang)
points(which(DD==0),cummax(cumsum(PL))[which(DD==0)]
       ,pch=4,col="red")
PL=PL[PL!=0]
length(PL[PL!=0])
length(PL[PL>0])/length(PL[PL!=0])
mean(PL[PL>0])/abs(mean(PL[PL<0]))
sum(PL[PL>=0])/abs(sum(PL[PL<0]))
tail(sort(diff(which(DD==0))),5)
View(cbind(as.numeric(Cl(STK))
           ,as.numeric(BBands(Cl(STK))[,1])
           ,as.numeric(row1)
           ,as.numeric(BBands(Cl(STK))[,3])
           ,as.numeric(row2)
           ,PL))
############short單股買賣
PL1=setNames(rep(0,length(Cl(STK))),time(STK))
PZ=0
row3=Cl(STK) >= BBands(Cl(STK))[,3]
row4=Cl(STK) <= BBands(Cl(STK))[,1]
for (m in 21:nrow(STK)){
  if (PZ==0 && row3[m-1]==0 && row3[m]==1){
    short=as.numeric(Cl(STK)[m])
    PZ=1
  }
  if (PZ==1 #&& row2[m-1]==0 
      && row4[m]==1){
    PL1[m]=as.numeric(Cl(STK)[m]-short)
    PZ=0  
  }
}
DD=cumsum(PL1)-cummax(cumsum(PL1))
yRang=range(DD,cumsum(PL1))

plot(DD,type="h",col="darkgreen",ylim=yRang)
par(new=T)
plot(cumsum(PL1),col="gold",lwd=2,type="h",ylim=yRang)
points(which(DD==0),cummax(cumsum(PL1))[which(DD==0)]
       ,pch=4,col="red")
PL1=PL1[PL1!=0]
length(PL1[PL1!=0])
length(PL1[PL1>0])/length(PL1[PL1!=0])
mean(PL1[PL1>0])/abs(mean(PL1[PL1<0]))
sum(PL1[PL1>=0])/abs(sum(PL1[PL1<0]))
tail(sort(diff(which(DD==0))),5)
View(cbind(as.numeric(Cl(STK))
           ,as.numeric(BBands(Cl(STK))[,1])
           ,as.numeric(row3)
           ,as.numeric(BBands(Cl(STK))[,3])
           ,as.numeric(row4)
           ,PL1))
############多空合併單股買賣
PL2=setNames(rep(0,length(Cl(STK))),time(STK))
PZ=0
row1=Cl(STK) <= BBands(Cl(STK))[,1]
row2=Cl(STK) >= BBands(Cl(STK))[,3]
for (m in 21:nrow(STK)){
  if (PZ==0 && row1[m-1]==0 && row1[m]==1){
    long=as.numeric(Cl(STK)[m])
    PZ=1
  }
  if (PZ==1 #&& row2[m-1]==0 
      && row2[m]==1){
    PL2[m]=as.numeric(Cl(STK)[m]-long)
    PZ=0  
  }
}

row3=Cl(STK) >= BBands(Cl(STK))[,3]
row4=Cl(STK) <= BBands(Cl(STK))[,1]
for (m in 21:nrow(STK)){
  if (PZ==0 && row3[m-1]==0 && row3[m]==1){
    short=as.numeric(Cl(STK)[m])
    PZ=1
  }
  if (PZ==1 #&& row4[m-1]==0 
      && row4[m]==1){
    PL2[m]=PL2[m]+as.numeric(Cl(STK)[m]-short)
    PZ=0  
  }
}

DD=cumsum(PL2)-cummax(cumsum(PL2))
yRang=range(DD,cumsum(PL2))
plot(DD,type="h",col="darkgreen",ylim=yRang)
par(new=T)
plot(cumsum(PL2),col="gold",lwd=2,type="h",ylim=yRang)
points(which(DD==0),cummax(cumsum(PL2))[which(DD==0)]
       ,pch=4,col="red")
PL2=PL2[PL2!=0]
length(PL2[PL2!=0])
length(PL2[PL2>0])/length(PL2[PL2!=0])
mean(PL2[PL2>0])/abs(mean(PL2[PL2<0]))
sum(PL2[PL2>=0])/abs(sum(PL2[PL2<0]))
tail(sort(diff(which(DD==0))),5)
View(cbind(as.numeric(Cl(STK))
           ,as.numeric(BBands(Cl(STK))[,1])
           ,as.numeric(row1)
           ,as.numeric(BBands(Cl(STK))[,3])
           ,as.numeric(row2)
           ,PL2))
