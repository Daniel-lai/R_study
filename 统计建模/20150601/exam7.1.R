#exam7.1
anova.tab<-function(fm) {
tab<-summary(fm)
k<-length(tab[[1]])-2
temp<-c(sum(tab[[1]][,1]),sum(tab[[1]][,2]),rep(NA,k))
tab[[1]]["Total",]<-temp
tab
}

#(1)
#ANOVA ANALYSIS
products<-data.frame(
x=c(115,116,98,83,
     103,107,118,116,
     73,89,85,97),
a=factor(rep(1:3,c(4,4,4))))
products.aov<-aov(x~a,data=products)
#方差分析表
anova.tab(products.aov)

#(2)
attach(products)
#各水平均值
mu<-c(mean(x[a==1]),mean(x[a==2]),mean(x[a==3]));mu
Sd<-c(sd(x[a==1]),sd(x[a==2]),sd(x[a==3]));Sd
t<-1.96
#区间估计
inv1<-c(mu[1]-t*Sd[1],mu[1]+t*Sd[1]);inv1
inv2<-c(mu[2]-t*Sd[3],mu[2]+t*Sd[2]);inv2
inv3<-c(mu[3]-t*Sd[3],mu[3]+t*Sd[3]);inv3

#(3)
pairwise.t.test(x,a,p.adjust.method="none")

