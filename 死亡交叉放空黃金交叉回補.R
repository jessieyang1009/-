library(quantmod)
STK=get(getSymbols("NFLX"))
chartSeries(STK)
row=SMA(Cl(STK),5)>SMA(Cl(STK),20)  #row是true/false向量

View(cbind(Cl(STK),SMA(Cl(STK),5),row)) #因為用5ma所以前四天沒有值
PL=setNames(rep(0,length(Cl(STK))),time(STK)) #損益向量（記錄每天賺多少賠多少）

short=PZ=0
for(m in 20:nrow(STK)){
  if(PZ==0 && row[m-1]==1 && row[m]==0){  #昨天是均線之下今天是均線之上(黃金交叉)
    short=as.numeric(Op(STK)[m+1])
    PZ=1
  }
  if(PZ==1 && row[m-1]==0 && row[m-1]==1){
    PL[m]=short-as.numeric(Op(STK)[m+1])
    PZ=0
  }
}
plot(cumsum(PL),type="l",col='red',lwd=2)