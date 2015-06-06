#拉希里法

lahiri<-function(data,n,N,...) {
x<-numeric(n)
y<-numeric(n)
k=1
while (k<=n) {
i<-sample(1:N)[1] #x的序号
ms<-max(data[,1]) #M*
j<-sample(1:ms)[1]
if (data[i,1]>=j) {
        x[k]<-data[i,1]
        y[k]<-data[i,2]
        k=k+1
}
}
data<-data.frame(x,y)
}