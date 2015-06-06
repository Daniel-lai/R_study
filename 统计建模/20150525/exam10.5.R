#exam10.5
queue<-function(lambda, mu1,mu2,mu3,mu4, T, S=1, K){
        if (K<S) K<-S
        k<-0; wt<-0; wn<-0; ws<-0
        tp<-0; nA<-0; t<-0
        r<-runif(1); tA<--1/lambda*log(r)
        tD<-rep(Inf, S); SS<-rep(0, S+1)
        repeat{
                t1<-if(SS[1]==0) Inf else min(tD)
                i1<-if(SS[1]==0) 1 else which.min(tD)
                k<-k+1; wt[k]<-t; wn[k]<-SS[1]
                if (tA < T){
                        ws[k]<-min(tA, t1+1)-t
                        if (tA < t1+1){
                                t<-tA; nA<-nA+1
                                r<-runif(1); tA<-t-1/lambda*log(r)
                                n<-SS[1]; SS[1]<-n+1
                                for (i in 1:S){
                                        if (SS[1+i]==0){
                                                SS[1+i]<-1
                                                r<-runif(1)
                                                if(i==1)
                                                {d=runif(1)
                                                 if(0<=d||d<=0.6)
                                                 {tD[i]<-t-1/mu1*log(r)}
                                                 else  tD[i]<-t-1/mu2*log(r)}
                                                if(i==2)
                                                {e=runif(1)
                                                 if(0<=e||e<=0.6)
                                                 {tD[i]<-t-1/mu3*log(r)}
                                                 else  tD[i]<-t-1/mu4*log(r)}
                                                break
                                        }
                                }
                                if (SS[1]==K){
                                        t1 <- min(tD)
                                        while (tA < t1+1){
                                                r<-runif(1); tA<-tA-1/lambda*log(r)
                                        }
                                }
                        }else{
                                t<-t1+1; n<-SS[1]; SS[1]<-n-1
                                if (n==1){
                                        SS[2:(S+1)]<-0; tD[1:S]<-Inf
                                }else if (n<=S){
                                        SS[1+i1]<-0; tD[i1]<-Inf
                                }else{
                                        r<-runif(1)
                                        if(i==1)
                                        {d=runif(1)
                                         if(0<=d||d<=0.6)
                                         {tD[i]<-t-1/mu1*log(r)}
                                         else  tD[i]<-t-1/mu2*log(r)}
                                        if(i==2)
                                        {e=runif(1)
                                         if(0<=e||e<=0.6)
                                         {tD[i]<-t-1/mu3*log(r)}
                                         else  tD[i]<-t-1/mu4*log(r)}
                                }
                        }
                }else{
                        ws[k]<- if( t1==Inf) 0 else t1+1-t
                        n<-SS[1]
                        if (n>0){
                                t<-t1+1; SS[1]<-n-1;
                                if (n==1){
                                        SS[2:(S+1)]<-0; tD[1:S]<-Inf
                                }else if (n<=S){
                                        SS[1+i1]<-0; tD[i1]<-Inf
                                }else{
                                        r<-runif(1)
                                        if(i==1)
                                        {d=runif(1)
                                         if(0<=d||d<=0.6)
                                         {tD[i]<-t-1/mu1*log(r)}
                                         else  tD[i]<-t-1/mu2*log(r)}
                                        if(i==2)
                                        {e=runif(1)
                                         if(0<=e||e<=0.6)
                                         {tD[i]<-t-1/mu3*log(r)}
                                         else  tD[i]<-t-1/mu4*log(r)}
                                }
                        }else
                                tp<-1
                }
                if (tp==1) break
        }
        data.frame(Ls=sum(ws*wn)/t, Ws=sum(ws*wn)/nA,
                   Plost=sum(ws[wn>=K])/t)
}
print(queue(lambda=4,mu1=6,mu2=9,mu3=5,mu4=7.5,T=600,S=2,K=6))
