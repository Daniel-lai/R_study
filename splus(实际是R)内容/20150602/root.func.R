root.func<-function() {
        print("enter an initial number and the order of formula")
        x0=scan()
        order=scan()
        formula_output<-function(order,coef) {
        orders=1:order
        ch_coef=as.character(coef)
        ch_orders=as.character(orders)
        s1=paste("+(",ch_coef[-1],sep="")
        s2=paste("*x^",ch_orders,sep="")
        s3=paste(s2,")",sep="")
        s4=paste(s1,s3,sep="")
        s5=ch_coef[1]
        for (i in 1:order) {
                s5=paste(s5,s4[i],sep="")
        }
        s5
        }
        coef_genr<-function(order,...) {
        j=TRUE
        coefficients=0
        while (j) {
                print("Input coefficient sequentially: constant, coef of x^1, coef of x^2, ...")
                coef=scan();
                if ((order+1)==length(coef)) {
                        j=FALSE
                        coefficients=coef
                } else {
                        print("Error! Try again. ")
                }
        }
        coefficients
        }
        trans2<-function(x,order,coefficients,...) {
        Order=0:order
        ch_fx=formula_output(order,coefficients)
        d_order=order-1
        d_Order=0:d_order
        d_coef=coefficients*Order
        ch_ffx=formula_output(d_order,d_coef[-1])
        fx=sum(x^Order*coefficients)
        ffx=sum(x^d_Order*d_coef)
        list(formula1=ch_fx,f0=fx,formula2=ch_ffx,f1=ffx)
        }
        root<-function(x0,order,...) {
        coef=coef_genr(order)
        x.int=x0
        dif=1e-06
        f=trans2(x0,order,coef)
        print(list(f[1],f[3]))
        count=1
        x0=x0
        x1=x0-f$f0/f$f1
        while (abs(x1-x0)>dif) {
        x0=x1
        f=trans2(x0,order,coef)
        x1=x0-f$f0/f$f1
        count=count+1
        if (count>100) {
                print("error! no root.")
                break
        }
        }
        list(x0=x.int,root=x1,steps=count,precision=dif)
        }
        root(x0,order)
}
