#exam5.1
x<-c(220,188,162,230,145,160,238,188,247,113,
     126,245,164,231,256,183,190,158,224,175)
mu<-225e9

#p-value func
p_val<-function(cdf,x,paramet=numeric(0),side=0) {
n<-length(paramet)
P<-switch(n+1,
          cdf(x),
          cdf(x,paramet),
          cdf(x,paramet[1],paramet[2]),
          cdf(x,paramet[1],paramet[2],paramet[3])
)
if (side<0) P
else if (side>0) 1-P
else if (P<1/2) 2*P
else 2*(1-P)
}

#正态总体均值检验
mean.test<-function(x,mu=0,sigma=-1,side=0) {
n<-length(x);xb<-mean(x)
if (sigma>0) {
z<-(xb-mu)/(sigma/sqrt(n))
P<-p_val(pnorm,z,side=side)
data.frame(mean=xb,df=n,Z=z,P_value=P)
} else {
t<-(xb-mu)/(sd(x)/sqrt(n))
P<-p_val(pt,t,paramet=n-1,side=side)
data.frame(mean=xb,df=n,T=t,P_value=P)
}
}

#假设检验
result<-mean.test(x,mu);result
if (result[4]<=0.05) print("apply H1") else print("apply H0")
