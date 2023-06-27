library(readxl)
currencies <- read_excel("D:/Programming Guide/data/currencies.xls")

currencies$reur = c(NA,100*diff(log(currencies$EUR)))
currencies$rgbp = c(NA,100*diff(log(currencies$GBP)))
currencies$rjpy = c(NA,100*diff(log(currencies$JPY)))

currencies = currencies[-1,]

library(vars)
VAR(currencies[c("reur","rgbp","rjpy")],p = 2)
VARselect(currencies[c("reur","rgbp","rjpy")], lag.max = 10)

var = VAR(currencies[c("reur","rgbp","rjpy")],p = 1)

grangertest(reur ~ rgbp, data = currencies)
grangertest(reur ~ rjpy, data = currencies)
causality(var,cause = c("rgbp","rjpy"))$Granger

grangertest(rgbp ~ reur, data = currencies)
grangertest(rgbp ~ rjpy, data = currencies)
causality(var,cause = c("reur","rjpy"))$Granger

grangertest(rjpy ~ reur, data = currencies)
grangertest(rjpy ~ rgbp, data = currencies)
causality(var,cause = c("reur","rgbp"))$Granger


par(cex.axis = 1.5, cex.lab = 1.5, lwd = 2)
ir = irf(var,n.ahead = 20)
plot(ir)
vd = fevd(var,n.ahead = 20)
plot(vd)

var_reverse = VAR(currencies[c("rjpy","rgbp","reur")],p = 1)
vd_reverse = fevd(var_reverse,n.ahead = 20)
plot(vd_reverse)

############################## Volatility modelling ##############################

library(vars)

garch11.spec = ugarchspec(mean.model = list(armaOrder=c(0,0)),variance.model = list(garchOrder=c(1,1),model="sGARCH"))
ugarchfit(spec,data=currencies$rjpy)

spec = ugarchspec(mean.model = list(armaOrder=c(0,0)),variance.model = list(garchOrder=c(1,1),model="eGARCH"))
ugarchfit(spec,data=currencies$rjpy)

spec = ugarchspec(mean.model = list(armaOrder=c(0,0)),variance.model = list(garchOrder=c(1,1),model="gjrGARCH"))
ugarchfit(spec,data=currencies$rjpy)

meanmodel = list(armaOrder=c(0,0),archm=T,archpow=2)
varmodel = list(garchOrder=c(1,1),model="sGARCH")
spec = ugarchspec(mean.model = meanmodel,variance.model = varmodel)
ugarchfit(spec,data=currencies$rjpy)

spec = ugarchspec(mean.model = list(armaOrder=c(0,0)),variance.model = list(garchOrder=c(1,1),model="eGARCH"))
fit = ugarchfit(spec,data = currencies$rjpy,out.sample = 700)

static_fc = ugarchforecast(fit,n.ahead=1,n.roll = 699)
dynamic_fc = ugarchforecast(fit,n.ahead = 700)
par(lwd=2,cex.axis = 2)
x_axis = currencies$Date[currencies$Date >= "2016-08-03"]
plot(x_axis,static_fc@forecast$sigmaFor,type="l",xlab="",ylab="",col="blue3")
lines(x_axis,dynamic_fc@forecast$sigmaFor,col="brown1")
legend("bottomleft", legend=c("Static", "Dynamic"),col=c("blue3","brown1"),lty= 1)


uspec = ugarchspec(mean.model = list(armaOrder=c(0,0)),variance.model = list(garchOrder=c(1,1),model="sGARCH"))
mspec = multispec(replicate(3,uspec))
cccspec = cgarchspec(mspec,VAR = F)
mod = cgarchfit(cccspec,data = currencies[c("reur","rgbp","rjpy")])
mod
mod@mfit$Rt


