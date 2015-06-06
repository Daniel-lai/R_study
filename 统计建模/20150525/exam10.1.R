#exam10.1
e<-0.01
#sqrt(2)/4+log(sqrt(2)/2+1)
#Mean Method
sigma1<-4/3-1.1446^2
n1<-round(1.96^2*sigma1/e^2)
result1<-function(n,...) {
        x<-runif(n)
        result<-sum(sqrt(1+x^2))/n
        result
}

#Monte Carlo
p<-1.1446/sqrt(2)
n2<-round(p*(1-p)*1.96^2/e^2)
result2<-function(n,...) {
        u<-runif(n)
        y<-runif(n,0,sqrt(2))
        k<-sum((sqrt(1+u^2))>=y)
        result<-k/n*sqrt(2)
        result
}



print(result2(n2))
print("Monte Carlo experiment times: ")
print(n2)

print(result1(n1))
print("Mean experiment times: ")
print(n1)
