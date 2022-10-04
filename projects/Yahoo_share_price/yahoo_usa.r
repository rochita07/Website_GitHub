#### code for Yahoo time series analysis of USA: ####

z=read.csv(file="D:/My Documents/Project_ Yahoo/a_yahoo usa _final.csv",header = T,sep=",")
cl=z$Close
plot(cl,main="Yahoo Closing share price in USA",xlab="time points",ylab="share
price",type="l",col=3)
lcl=log(cl)
plot(lcl,main="Log Yahoo Closing share price in USA",xlab="time points",ylab="log share
price",type="l",col=4)
y=lcl ## study variable
acf(y,lag.max=1000,main="ACF of log-yahoo")
pacf(y,lag.max=1000,main="PACF of log-yahoo")
##------------ check of trend ------------##
library(Kendall)
MannKendall(y) ## monotonic trend present
d1=diff(y)
plot(d1,main="1st order difference data",xlab="time pt.",ylab="1st order difference")
abline(h=0,col=2)
MannKendall(d1) ## monotonic trend absent ## hence we can treat 1st order diff data as detrended
data

##------------ check of seasonality ---------------##
m1=c(d1[1:5])
m2=c(d1[6:10])
m3=c(d1[11:15])
m4=c(d1[16:20])
m5=c(d1[21:25])
m6=c(d1[26:30])
m=cbind(m1,m2,m3,m4,m5,m6)
matplot(m,type="l",main="Week wise plot of detrend data",ylab="log share price",xlab="time
points",col=c(1,2,3,4,5,6))
legend(locator(1),col=c(1,2,3,4,5,6),legend=c("1st week","2nd week","3rd week","4th week","5th
week","6th week"),pch=15)
d2=d1 ## data is sorted along month.
jan=c(d2[1:8],d2[242:261],d2[494:513],d2[744:764],d2[996:1016],d2[1248:1267],d2[1500:1515])
feb=c(d2[9:27],d2[262:280],d2[514:533],d2[765:783],d2[1017:1035],d2[1268:1286])
mar=c(d2[28:50],d2[281:303],d2[534:554],d2[784:803],d2[1036:1056],d2[1287:1308])
apr=c(d2[51:71],d2[304:323],d2[555:575],d2[804:825],d2[1057:1077],d2[1309:1329])
may=c(d2[72:91],d2[324:344],d2[576:597],d2[826:847],d2[1078:1098],d2[1330:1349])
june=c(d2[92:113],d2[345:366],d2[598:618],d2[848:867],d2[1099:1118],d2[1350:1371])
july=c(d2[114:134],d2[367:386],d2[619:639],d2[868:889],d2[1119:1141],d2[1372:1393])
aug=c(d2[135:156],d2[387:409],d2[640:662],d2[890:911],d2[1142:1162],d2[1394:1414])
sept=c(d2[157:177],d2[410:430],d2[663:681],d2[912:931],d2[1163:1184],d2[1415:1435])
oct=c(d2[178:198],d2[431:451],d2[682:702],d2[932:954],d2[1185:1206],d2[1436:1457])
nov=c(d2[199:219],d2[452:472],d2[703:723],d2[955:974],d2[1207:1225],d2[1458:1477])
dec=c(d2[220:241],d2[473:493],d2[724:743],d2[975:995],d2[1226:1247],d2[1478:1499])
boxplot(jan,feb,mar,apr,may,june,july,aug,sept,oct,nov,dec,main="Month wise boxplot on detrend
data",xlab="month")
kruskal.test(list(jan,feb,mar,apr,may,june,july,aug,sept,oct,nov,dec))
acf(d1,lag.max=1000,main="ACF of stationary data")
pacf(d1,lag.max=1000,main="PACF of stationary data")

##-------------------- MEAN stationary---------------##
library(forecast)
for(i in 0:4)
{ for(k in 0:4)
{ b1=arima(y,order=c(i,1,k))
print(i)
print(k)
print(AIC(b1))
}} ## on min AIC, ARIMA(3,1,1) is selected
f1=arima(y,order=c(1,1,3))
f2=fitted(f1) ##predicted value
g=cbind(y,f2)
matplot(g,type="l",col=c(3,2),main="Observed data & fitted
ARIMA(1,1,3)",ylab="log_yahoo",xlab="time pts")
legend(locator(1), legend =c("Observed data","Fitted value"),col=c(3,2), pch=15)
resid=y-f2 ## residual of ARIMA(1,1,3)
plot(resid,main="Residual in ARIMA(1,1,3)",ylab="residual",col=1)
acf(resid,main="ACF of residual of ARIMA(1,1,3)",lag.max=1000)
pacf(resid,main="PACF of residual of ARIMA(1,1,3)",lag.max=1000)
library(portes)

portest(resid,test="LjungBox") ## white noise is absent in ARIMA(3,1,1) residual
library(FinTS) ## test for arch/garch ##
ArchTest(resid,lags=1)
##----------------- VARIANCE stationary --------------##
library(tseries)
for(i in 0:3)
{ for(j in 1:3)
{ ff=garch(resid,order=c(i,j),mse="uncond",trace=FALSE)
print(i)
print(j)
print(AIC(ff))
}} ## hence on basis of min aic garch(1,1)
f=garch(resid,order=c(1,1),mse="uncond",trace=FALSE)
print(AIC(f))
library(fGarch)
fit=garchFit(~ garch(1,1),data=resid)
fit

##---------- Generate plot of Log Price, 95% Upper and Lower limit -------##
f2=fitted.values(f1) ## fitted mean by ARIMA(3,1,1)
ht=f$fit[,1]^2 ## fitted conditional standard deviation by GARCH(1,1)
plot(ht,main='Conditional variance fitted by by GARCH(1,1)',ylab="Conditional variance",col=1)
library(moments)
kurtosis(resid)
qqnorm(resid,main='ARIMA(1,1,3) Residuals')
qqline(resid,col=2) ## plot shows distn. of ARIMA(3,1,1) residual differs from Normal distn.
anscombe.test(resid, alternative = "greater") ## Anscombe-Glynn test of kurtosis for normal samples
anscombe.test(resid, alternative = "less")
kur=kurtosis(resid)
nu=round((6/kur)+4)
nu ## parameter of 't'-distn ( to be fitted in ARMA residual)
b=qt(.95, df=nu, ncp=0, lower.tail = TRUE, log.p = FALSE) ## upper 95% value of t(nu=6) distn.
low=f2-b*sqrt(ht) ## lower conf. limit
high=f2+b*sqrt(ht) ## upper conf. limit
plot(y,type="l",main='Log yahoo with C.I.',xlab="time",ylab="log-yahoo",col=3)
lines(low,col=2)
lines(high,col=4)
legend(locator(1), legend =c("Log yahoo","Lower Conf. limit","Upper Conf. limit"),col=c(3,2,4),
pch=15)

##---------- FORECAST----------------##

f3=forecast(f1,h=10) ## mean forecast by ARIMA(3,1,1) ## pr(say)
p1=predict(fit, n.ahead = 10,plot=T) ## std. devtn forecast by GARCH(1,1)
pr=c(3.400090,3.399856,3.399360,3.399041,3.398836,3.398705,3.398620,3.398566,3.398531,3.39850
9 )
pred=exp(pr) ## predicted log-yahoo
lcll=pr-b*p1[,3] ## predicted lower conf limit
ucl=pr+b*p1[,3] ## predicted upper conf limit
lcll=exp(lcll)
ucl=exp(ucl)