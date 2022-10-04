#### code for Regression Analysis: ####

z_u=read.csv(file="D:/My Documents/Project_ Yahoo/a_yahoo usa _final.csv",header = T,sep=",")
cl_u=z_u$Close
lcl_u=log(cl_u)
y_u=lcl_u ## y_usa
z_j=read.csv(file="D:/My Documents/Project_ Yahoo/a_yahoo japan _final.csv",header = T,sep=",")
cl_j=z_j$Close
lcl_j=log(cl_j)
y_j=lcl_j ## y_jap
y1=cbind(y_j,y_u)
zx=read.csv(file="D:/My Documents/Project_ Yahoo/exogeneous.csv",header = T,sep=",")
nas=zx$nasdaq ## exogeneous variables
nik=zx$nikkei
exc=zx$exchange
mis=cbind(nas,nik,exc) ## MICE algo to estimate missing values in exchange ratio.
library(mice)
mc=mice(mis)
mce=complete(mc)
exc1=mce[,3]
plot(nas,main="NASDAQ Index",xlab="time",ylab="nasdaq index",type="l",col=4)
plot(nik,main="NIKKEI Index",xlab="time",ylab="nikkei index",type="l",col=2)
plot(exc1,main="Yen-Dollar Exchange Rate",xlab="time",ylab="exchange rate",type="l",col=3)
log_nas=log(nas)
log_nik=log(nik)
log_exc=log(exc1)
d11=cbind(log_nas,log_nik,log_exc)
library(vars) ## fit suitable VAR model and causality test
VARselect(cbind(y_u,y_j),exogen =d11, lag.max=10, type="both")$selection
vz1=VAR(cbind(y_u,y_j),p=1,type = "both", exogen =d11, lag.max =NULL,ic = "AIC")
summary(vz1)
causality(vz1,cause="y_j")
causality(vz1,cause="y_u") ## y_j and y_u are instantaneous causal
## -------- Simultaneous equation ----------- ##
fit_y_j=predict(lm(y_j~log(nik)+log(nas)+log(exc1)))
fit_y_u=predict(lm(y_u~log(nik)+log(nas)+log(exc1)))
cbind(fit_y_j,y_j,fit_y_u,y_u)
l_j_1=lm(y_j~fit_y_u+log(nik)+log(exc1))
l_u_1=lm(y_u~fit_y_j+log(nas)+log(exc1))
c2=coef(l_j_1) #japan
c1=coef(l_u_1) #USA
f_y_j=predict(lm(y_j~fit_y_u+log(nik)+log(exc1)))
f_y_u=predict(lm(y_u~fit_y_j+log(nas)+log(exc1)))
matplot(cbind(exp(f_y_j),exp(y_j)),main="Observed and Predicted Yahoo share in
JAPAN",xlab="time",ylab="share price",col=c(4,2),type="l")
legend(locator(1),col=c(4,2),legend=c("observed share","predicted share"),pch=15)
matplot(cbind(exp(f_y_u),exp(y_u)),main="Observed and Predicted Yahoo share in
USA",xlab="time",ylab="share price",col=c(4,2),type="l")
legend(locator(1),col=c(4,2),legend=c("observed share","predicted share"),pch=15)
# structural form # a*y=d+b*x , x=(log(nas),log(nik),log(exc))
a=matrix(c(1,-c1[2],-c2[2],1),nrow=2,byrow=T)
d=matrix(c(c1[1],c2[1]),nrow=2)
b=matrix(c(c1[3],0,c1[4],0,c2[3],c2[4]),nrow=2,byrow=T)
# reduced form # y=d1+b1*x
d1=solve(a)%*%d
b1=solve(a)%*%b
for(j in 1:1515) ## for cross check
{
w=d1+b1%*%d11[j,]
print(w)
}
##---- prediction--------##
data=read.csv(file="D:/My Documents/Project_ Yahoo/prediction.csv",header = T,sep=",")
data
x=cbind(log(data$nasdaq),log(data$nikkei),log(data$exchange))
x=t(x)
for(j in 1:10)
{
y=d1+b1%*%x[,j]
y=exp(y)
print(j)
print(y)
}
cbind(data$Japan,data$USA)
