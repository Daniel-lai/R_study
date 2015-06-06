
trans<-function(x) {
        f0=2*x^4+3*x^3+x^2-100
        f1=8*x^3+9*x^2+2*x
        list(f0=f0,f1=f1)
}

root.func<-function(x0) {
        x.int=x0
        dif=1e-06
        f=trans(x0)
        count=1
        x0=x0
        x1=x0-f$f0/f$f1
        while (abs(x1-x0)>dif) {
        x0=x1
        f=trans(x0)
        x1=x0-f$f0/f$f1
        count=count+1
        if (count>100) {
                print("error! no root.")
                break
        }
        }
        list(x0=x.int,root=x1,steps=count,precision=dif)
}