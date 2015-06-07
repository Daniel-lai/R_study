#rnorm、runif、rpois抽样，并用所抽的样本，计算样本均值和样本方差，极大似然估计，矩估计
sampling<-function(n,method,...) {
        #print("method='rnorm','runif','rpois'")
        if (method==1 | method=="rnorm") {
                print("do you decide to use default parameters for Normal distribution? enter y or n")
                i=scan("",what="")
                if (i=="n") {
                        print("enter parameters for Normal distribution, mean and sd")
                        parameters=scan("")
                        s=rnorm(n,parameters[1],parameters[2])
                } else s=rnorm(n)
        }
        if (method==2 | method=="runif") {
                print("do you decide to use default parameters for Uniform distribution? enter y or n")
                i=scan("",what="")
                if (i=="n") {
                        print("enter parameters for Uniform, min and max")
                        parameters=scan("")
                        s=runif(n,parameters[1],parameters[2])
                } else s=runif(n)
        }
        if (method==3 | method=="rpois") {
                print("enter parameter lambda for Poisson distribution.")
                lambda=scan("")
                s=rpois(n,lambda)
        }
        list(mean=mean(s),var=var(s))
}