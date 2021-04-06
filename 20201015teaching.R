library(quantmod)

par(mfrow=c(1,3))

STK=na.omit(get(getSymbols("2330.TW")))
STK=to.weekly(STK)

chartSeries(to.weekly(STK)["2010::2020"])
addSMA(3,col="white")
addEMA(5,col="lightblue")  #影響程度會因為距時間越久而影響力下降
?BBands
View(BBands(Cl(STK),n=10,sd=1))
addBBands()


class(STK)
class(time(STK))
View(STK)
tail(cbind(Cl(STK),runMean(Cl(STK),3)))
#?SMA
head(SMA(Cl(STK),3))



STK=as.matrix(STK) #改成陣列模式
PLts=xts(numeric(length(time(STK))),time(STK))
#產生時間序列的損益向量
for (m in as.character(time(STK)))
{
  PLts[m]=STK[m,4]-STK[m,1]
}
plot(cumsum(PLts))

#原始
PL=setNames(numeric(nrow(STK)),rownames(STK))

for (m in rownames(STK)) #計算總共有幾列日期
{
  PL[m]=STK[m,4]-STK[m,1]
}
plot(cumsum(PL),type = "l",col="red",lwd=2)
abline(h=0,col="green")


for(m in 2:nrow(STK)){
  if(Op(STK)[m]<=Cl(STK)[m-1]){
    PL[m]=STK[m,4]-STK[m,1]
  }
}
plot(cumsum(PL),type = "l",col="green",lwd=2)
abline(h=0,col="green")
