#### code for Yahoo time series analysis of Japan: ####

z=read.csv(file="D:/My Documents/Project_ Yahoo/a_yahoo japan _final.csv",header = T,sep=",")
cl=z$Close
plot(cl,main="Yahoo Closing share price in Japan",xlab="time points",ylab="yahoo share
price",type="l",col=3)
lcl=log(cl)
plot(lcl,main="Log Yahoo share price in Japan",xlab="time points",ylab="log yahoo share
price",type="l",col=4)
y=lcl ## study variable
acf(y,lag.max=1000,main="ACF of log yahoo share price")
pacf(y,lag.max=1000,main="PACF of log yahoo share price")
##------------ check of trend ------------##
library(Kendall)
MannKendall(y) ## monotonic trend present
d1=diff(y)
plot(d1,main="1st order difference data",xlab="time pt.",ylab="1st order difference") ## outlier present
abline(h=0,col=2) ## outlier present in the plot .hence we opt for piece-wise regression to fit the trend.
g1=cbind(y[1:389],z$time.pt[1:389])
g2=cbind(y[390:937],z$time.pt[390:937])
g3=cbind(y[938:1515],z$time.pt[938:1515])
lm1=lm(g1[,1]~g1[,2])
lm2=lm(g2[,1]~g2[,2]+I(g2[,2]^2))

lm3=lm(g3[,1]~g3[,2]+I(g3[,2]^2))
l=c(predict(lm1),predict(lm2),predict(lm3))
plot(y,type="l",main="Observed value with fitted piece-wise trend",xlab="time pt.",ylab="logyahoo",
col=3)
lines(l,col=2)
legend(locator(1), legend =c("Observed value","Fitted value"),col=c(3,2), pch=15)
d2=y-l ## detrended value
plot(d2,main="Detrend through piece-wise regression",xlab="time pt.",ylab="detrend value")
abline(h=0,col=2)
MannKendall(d2) ## monotonic trend absent
##------------ check of seasonality ---------------##
k1=c(d2[1:5])
k2=c(d2[6:10])
k3=c(d2[11:15])
k4=c(d2[16:20])
k5=c(d2[21:25])
k6=c(d2[26:30])
k=cbind(k1,k2,k3,k4,k5,k6)
matplot(k,type="l",main="Week wise plot of detrended data",ylab="log share price",xlab="time
pt.",col=c(1,2,3,4,5,6))
legend(locator(1),col=c(1,2,3,4,5,6),legend=c("1st week","2nd week","3rd week","4th week","5th
week","6th week"),pch=15)

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
## removing seasonality ##
s1=mean(jan)
s2=mean(feb)
s3=mean(mar)
s4=mean(apr)
s5=mean(may)
s6=mean(june)
s7=mean(july)
s8=mean(aug)
s9=mean(sept)
s10=mean(oct)
s11=mean(nov)
#s12=mean(dec)
s12=0-sum(s1,s2,s3,s4,s5,s6,s7,s8,s9,s10,s11)
detrend=d2
d2=rep(0,1515)
d2[1:8]=s1
d2[242:261]=s1
d2[494:513]=s1
d2[744:764]=s1
d2[996:1016]=s1
d2[1248:1267]=s1
d2[1500:1515]=s1
d2[9:27]=s2
d2[262:280]=s2
d2[514:533]=s2
d2[765:783]=s2
d2[1017:1035]=s2
d2[1268:1286]=s2
d2[28:50]=s3
d2[281:303]=s3
d2[534:554]=s3
d2[784:803]=s3
d2[1036:1056]=s3
d2[1287:1308]=s3
d2[51:71]=s4
d2[304:323]=s4
d2[555:575]=s4
d2[804:825]=s4
d2[1057:1077]=s4
d2[1309:1329]=s4
d2[72:91]=s5
d2[324:344]=s5
d2[576:597]=s5
d2[826:847]=s5
d2[1078:1098]=s5
d2[1330:1349]=s5
d2[92:113]=s6
d2[345:366]=s6
d2[598:618]=s6
d2[848:867]=s6
d2[1099:1118]=s6
d2[1350:1371]=s6
d2[114:134]=s7
d2[367:386]=s7
d2[619:639]=s7
d2[868:889]=s7
d2[1119:1141]=s7
d2[1372:1393]=s7
d2[135:156]=s8
d2[387:409]=s8
d2[640:662]=s8
d2[890:911]=s8
d2[1142:1162]=s8
d2[1394:1414]=s8
d2[157:177]=s9
d2[410:430]=s9
d2[663:681]=s9
d2[912:931]=s9
d2[1163:1184]=s9
d2[1415:1435]=s9
d2[178:198]=s10
d2[431:451]=s10
d2[682:702]=s10
d2[932:954]=s10
d2[1185:1206]=s10
d2[1436:1457]=s10
d2[199:219]=s11
d2[452:472]=s11
d2[703:723]=s11
d2[955:974]=s11
d2[1207:1225]=s11
d2[1458:1477]=s11
d2[220:241]=s12
d2[473:493]=s12
d2[724:743]=s12
d2[975:995]=s12
d2[1226:1247]=s12
d2[1478:1499]=s12
seas_ind=d2
deseason=detrend-seas_ind
plot(deseason,main="Detrended,deseasonalised data",xlab="time",ylab="detrend deseasonalised")
abline(h=0,col=2)
d2=deseason
d2 ## detrended, deseasonalised data

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
boxplot(jan,feb,mar,apr,may,june,july,aug,sept,oct,nov,dec,main="Boxplot on detrended &
deseasonalised data",xlab="month")
acf(d2,lag.max=1000,main="ACF of stationary data")
pacf(d2,lag.max=1000,main="PACF of stationary data")

##---------------- MEAN stationary---------------##
library(forecast)
for(i in 0:4)
{ for(k in 0:4)
{ b2=arima(d2,order=c(i,0,k))
print(i)
print(k)
print(AIC(b2))
}} ## on min AIC, ARIMA(2,0,4) is selected.
f1=arima(d2,order=c(2,0,2))
f2=fitted(f1) ##predicted value
g=cbind(d2,f2)
matplot(g,type="l",col=c(3,2),main="Stationary & fitted ARMA(2,2)",ylab="stationary",xlab="time
pts")
legend(locator(1), legend =c("Stationary value","Fitted value"),col=c(3,2), pch=15)
resid=d2-f2 ## residual of ARMA(2,2)
plot(resid,main="Residual in ARMA(2,2)",ylab="residual")
acf(resid,main="ACF of residual of ARMA(2,2)",lag.max=1000)
pacf(resid,main="PACF of residual of ARMA(2,2)",lag.max=1000)
library(portes)
portest(resid,test="LjungBox") ## white noise is absent in ARMA(2,2) residual
## test for arch/garch ##
library(FinTS)
ArchTest(resid,lags=1)
##------------ VARIANCE stationary --------------##
library(tseries)
for(i in 0:3)
{ for(j in 1:3)
{ ff=garch(resid,order=c(i,j),mse="uncond",trace=FALSE)
print(i)
print(j)
print(AIC(ff))
}} ## hence on basis of min aic garch(1,1)
f=garch(resid,order=c(1,1),mse="uncond",trace=FALSE) #garch(0,2)=arch(2)
summary(f)
library(fGarch)
fit=garchFit(~ garch(1,1),data=resid)
summary(fit)
##---------- Generate plot of Log Price, 95% Upper and Lower limit -------##
f2=fitted.values(f1) ## fitted mean by ARMA(2,2)
ht=f$fit[,1]^2 ## fitted conditional standard deviation by GARCH(1,1)
plot(ht,main='Conditional variance fitted by by GARCH(1,1)',ylab="Conditional variance")
library(moments)
kurtosis(resid)
qqnorm(resid,main='ARMA(2,2) Residuals')
qqline(resid,col=2) ## plot shows distn. of ARMA(2,1) residual differs from Normal distn.
anscombe.test(resid, alternative = "greater") ## Anscombe-Glynn test of kurtosis for normal samples
anscombe.test(resid, alternative = "less")
kur=kurtosis(resid)
nu=round((6/kur)+4)
nu ## parameter of 't'-distn ( to be fitted in ARMA residual)
b=qt(.95, df=nu, ncp=0, lower.tail = TRUE, log.p = FALSE) ## upper 95% value of t(nu=6) distn.
low=f2-b*sqrt(ht) ## lower conf. limit
high=f2+b*sqrt(ht) ## upper conf. limit
plot(d2,type="l",main='Log yahoo stationary with C.I.',xlab="time",ylab="stationary",ylim=c(-
.25,.25),col=3)
lines(low,col=2)
lines(high,col=4)
legend(locator(1), legend =c("Log yahoo stationary","Lower Conf. limit","Upper Conf.
limit"),col=c(3,2,4), pch=15)
plot(y,type="l",main='Log yahoo with C.I.',xlab="time",ylab="log-yahoo",col=3)
lines(low+l+seas_ind,col=2)
lines(high+l+seas_ind,col=4)
legend(locator(1), legend =c("Log yahoo","Lower Conf. limit","Upper Conf. limit"),col=c(3,2,4),
pch=15)
##---------------- FORECAST----------------##
p1=predict(fit, n.ahead = 10,plot=T) ## std. devtn forecast by GARCH(1,1)
f3=forecast(f1,h=10) ## mean forecast by ARMA(2,2) ## pr_1(say)
pr_1=c(-0.09902606,-0.09726107,-0.09003538,-0.08862722,-0.08185825,-0.08077056,-0.07442120,-
0.07362101,-0.06765739,-0.06711477) # predicted stationary
w=coef(lm3)
seq=c(1516:1525)
pr_2=w[1]+w[2]*seq+w[3]*seq^2 ## predicted trend
pr_3=c(rep(s1,3),rep(s2,7)) ## seasonal indices accdn time pt.
pr_4=pr_1+pr_2+pr_3
pred=exp(pr_4) ## predicted log-yahoo
lcll=pr_4-b*p1[,3] ## predicted lower conf limit
ucl=pr_4+b*p1[,3] ## predicted upper conf limit
lcll=exp(lcll)
ucl=exp(ucl)