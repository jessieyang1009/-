#install.packages("quantmod")
library(quantmod)

STK=na.omit(get(getSymbols("BBBY")))
STK=to.monthly(STK)
#PL=Cl(STK)-Op(STK) #收盤-開盤的損益
#PL0=Cl(STK)*(1-0.001425/2)-Op(STK)*(1-0.001425/2-0.003) #手續費半價
PL1=Cl(STK)*(1-0.001425)-Op(STK)*(1-0.001425-0.003) #手續費加稅
#View(cbind(Op(STK),Cl(STK),PL))#用view check是否有算錯
#View(cbind(Op(STK),Cl(STK),PL0))
#View(cbind(Op(STK),Cl(STK),PL1))
#sum(PL)
#sum(PL0)
sum(PL1)
#plot(cumsum(PL)) #累積歷年來的
#plot(cumsum(PL0))
plot(PL1)
plot(cumsum(PL1))

