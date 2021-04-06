library(quantmod)
STK=get(getSymbols("NFLX"))
chartSeries(STK)
row=Cl(STK)>SMA(Cl(STK),5)  #row是true/false向量

View(cbind(Cl(STK),SMA(Cl(STK),5),row)) #因為用5ma所以前四天沒有值
PL=setNames(rep(0,length(Cl(STK))),time(STK)) #損益向量（記錄每天賺多少賠多少）
m=6
while(m<nrow(STK)){  #nrow(STK)是指總共幾天
if(row[m-1]==0&&row[m]==1){  #昨天是均線之下今天是均線之上
long=as.numeric(Op(STK)[m+1]) #long（買）在隔天開盤價
while(row[m]==1&&m<nrow(STK)){m=m+1} #選賣出時間
PL[m]=as.numeric(Op(STK)[m+1])-long #賣出
  }
m=m+1
}
plot(cumsum(PL),type="l",col='red',lwd=2)
####################### while改for
long=PZ=0
for(m in 6:nrow(STK)){
  if(PZ==0 && row[m-1]==0 && row[m]==1){  #昨天是均線之下今天是均線之上(黃金交叉)
    long=as.numeric(Op(STK)[m+1])
    PZ=1
  }
  if(PZ==1 && row[m-1]==1 && row[m-1]==0){
    PL[m]=as.numeric(Op(STK)[m+1])-long
    PZ=0
  }
}
plot(cumsum(PL),type="l",col='red',lwd=2)


#################### #5ma死亡交叉20ma放空，黃金交叉回補買進
library(quantmod)
STK=get(getSymbols("AAPL"))
row=SMA(Cl(STK),5)>SMA(Cl(STK),20) 
PL=setNames(rep(0,length(Cl(STK))),time(STK))

m=21
while(m<nrow(STK)){  #nrow(STK)是指總共幾天
  if(row[m-1]==0 && row[m]==1){  
    short=as.numeric(Op(STK)[m+1]) 
    while(row[m]==1 && m<nrow(STK)){m=m+1} 
    PL[m]=as.numeric(Op(STK)[m+1]) -short
  }
  m=m+1
}
plot(cumsum(PL),type="l",col='red',lwd=2)
