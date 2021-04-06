#install.packages("quantmod")
library(quantmod)
rm(list = ls())
getSymbols("AAPL")
View(AAPL)
chartSeries(to.monthly(AAPL["2020"])
            ,theme='white'
            ,up.col = "red",dn.col = "green")#預設皆為to.daily



#STK=get(getSymbols("^TWII"))
#getSymbols("^TWII", from="2010-1-1")
#STK=to.weekly(TWII)
#View(STK)
#chartSeries(STK["2020"],theme='white',up.col = "red",dn.col = "green")



STK=na.omit(get(getSymbols("2317.TW")))#取台股
STK=to.weekly(STK) #變月份
View(STK) #看開高低收量還原權值
chartSeries(STK["2020"]
            ,theme='white'
            ,up.col = "red",dn.col = "green")
Op(STK)
Pi(STK)
Lo(STK)
(Hi(STK)+Lo(STK))/2
plot(Cl(STK))

#PL=Hi(STK)-Lo(STK) #高-低的損益
PL=Cl(STK)-Op(STK) #收盤-開盤的損益
View(PL)
#View(cbind(Op(STK),Cl(STK),PL))#用view check是否有算錯

sum(PL)
plot(cumsum(PL)) #累積歷年來的


