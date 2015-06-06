#example
#e1
source("buffon.R")
e1<-buffon(100000, l=0.8, a=1)
print(e1)

#e2
source("queue3.R")
e2<-queue3(lambda=6, mu=5, T=1000, K=4)
print(e2)
