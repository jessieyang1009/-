library(quantmod)
STK=ma.omit(get(getSymbols("AAPL")))
#STK=to.monthly(STK)

chartSeries(to.monthly(STK)["2010::2020"])
addSMA(3,col="white")
addEMA(5,col="lightblue")  #影響程度會因為距時間越久而影響力下降
STK=as.matrix(STK)
addBBands()


     
     