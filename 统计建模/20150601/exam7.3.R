#exam7.3
#(1) shapiro.test()
mouse<-data.frame(
x=c(30,27,35,35,29,33,32,36,26,41,33,31,
    43,45,53,44,51,53,54,37,47,57,48,42,
    82,66,66,86,56,52,76,83,72,73,59,53),
a=factor(rep(1:3,c(12,12,12)))
)
attach(mouse)
#level1
shapiro.test(x[a==1])
#level2
shapiro.test(x[a==2])
#level3
shapiro.test(x[a==3])
#3种水平下均为正态

#(2) bartlett.test(x~a,data=data)
bartlett.test(x~a,data=mouse)
#p值<0.05,认为各处理组的数据不是等方差的