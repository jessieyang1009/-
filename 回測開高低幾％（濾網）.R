library(quantmod)
par(mfrow=c(1,3))
STK=na.omit(get(getSymbols("2492.TW")))
STK=to.monthly(STK)
STK=as.matrix(STK)
View(STK)
###原始
PL=setNames(numeric(nrow(STK)),rownames(STK))

#for (m in rownames(STK)){
for (m in 2:nrow(STK)){
  # if (Op(STK)[m]<Cl(STK)[m-1]){
  PL[m]=STK[m,4]-STK[m,1]  #關盤-開盤
  # }
}
plot(cumsum(PL),type="l",col="blue",lwd=2)
abline(h=0,col="green")


###開高
PL=setNames(numeric(nrow(STK)),rownames(STK))
#for (m in rownames(STK)){
for (m in 2:nrow(STK)){
  if (Op(STK)[m]-Cl(STK)[m-1]>0.01*Cl(STK)[m-1]){
    PL[m]=STK[m,4]-STK[m,1]
  }
}
plot(cumsum(PL),type="l",col="red",lwd=2)
abline(h=0,col="green")


PL=setNames(numeric(nrow(STK)),rownames(STK))
#for (m in rownames(STK)){
for (m in 2:nrow(STK)){
  if (Op(STK)[m]-Cl(STK)[m-1]<=0.01*Cl(STK)[m-1]){
    PL[m]=STK[m,4]-STK[m,1]
  }
}
plot(cumsum(PL),type="l",col="green",lwd=2)
abline(h=0,col="green")
