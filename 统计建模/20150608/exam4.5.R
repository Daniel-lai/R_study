#exam4.5
x<-c(54,67,68,78,70,66,67,70,65,69)
#点估计，极大似然估计法MLE
mu1<-mean(x)
sigma2<-var(x)
list(mu=mu1,sigma2=sigma2)

#均值μ的区间估计
inv_est<-function(x,sigma=-1,alpha=0.05,...) {
n<-length(x);xb<-mean(x)
if (sigma>=0) {
tmp<-sigma/sqrt(n)*qnorm(1-alpha/2);df<-n
}
else {
tmp<-sd(x)/sqrt(n)*qt(1-alpha/2,n-1);df<-n-1
}
data.frame(mean=xb,df=df,a=xb-tmp,b=xb+tmp)
}

#0.95区间估计
inv_est(x)

#单双侧区间估计
inv_est2<-function(x,sigma=-1,side=0,alpha=0.05) {
n<-length(x);xb<-mean(x)
if (sigma>=0) {
if (side<0) {
tmp<-sigma/sqrt(n)*qnorm(1-alpha)
a<-Inf;b<-xb+tmp
} else if (side>0) {
tmp<-sigma/sqrt(n)*qnorm(1-alpha)
a<-xb-tmp;b<-Inf
} else {
tmp<-sigma/sqrt(n)*qnorm(1-alpha/2)
a<-xb-tmp;b<-xb+tmp
}
df<-n
} else {
if (side<0) {
tmp<-sd(x)/sqrt(n)*qt(1-alpha,n-1)
a<--Inf;b<-xb+tmp
} else if (side>0) {
tmp<-sd(x)/sqrt(n)*qt(1-alpha,n-1)
a<-xb-tmp;b<-Inf
} else {
tmp<-sd(x)/sqrt(n)*qt(1-alpha/2,n-1)
a<-xb-tmp;b<-xb+tmp
}
df<-n-1
}
data.frame(mean=xb,df=df,a=a,b=b)
}

#上限估计,side=-1
result<-inv_est2(x,side=-1);result
if (result[1]<72) print("lower!") else print("upper!")
