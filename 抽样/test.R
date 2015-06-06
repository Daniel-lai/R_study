cat("提示：请按照指定格式新建一个csv文件\t切换到该文件目录下,\n")
cat("敲入命令[变量名]<-read.csv(\"[文件名].csv\"),然后调用listing(变量名)。")
cat("\n表格规范：x(或m)\ty\tn\tN\tX(或M0)\tt\n")
cat("或者用data<-edit(data)直接编辑数据表，然后调用lising(data)。祝旅途愉快！\n")

data<-data.frame(x=0,y=0,n=0,N=0,X=0,t=0)

#PPS
hh<-function(data,...) {
        m<-data[,1]
        y<-data[,2]
        n<-data[1,3]
        N<-data[1,4]
        M0<-data[1,5]
        t<-data[1,6]
        y_hh<-M0/n*sum(y/m)
        var_y_hh<-M0^2/n*sum((y/m-y_hh/M0)^2)/(n-1)
        se_y_hh<-sqrt(var_y_hh)
        inv_l<-y_hh-t*se_y_hh
        inv_u<-y_hh+t*se_y_hh
        cv_y_hh<-se_y_hh/y_hh
        result<-data.frame(y_hh,var_y_hh,se_y_hh,cv_y_hh,inv_l,inv_u)
}

#回归
lr<-function(data,...) {
        x<-data[,1]
        y<-data[,2]
        n<-data[1,3]
        N<-data[1,4]
        X<-data[1,5]
        t<-data[1,6]
        x_mean<-mean(x)
        y_mean<-mean(y)
        X_mean<-X/N
        f<-n/N
        b<-cov(y,x)/cov(x,x)
        y_lr<-N*(y_mean-b*(x_mean-X_mean))
        var_y_lr<-N^2*(1-f)/n*sum(((y-y_mean)-b*(x-x_mean))^2)/(n-2)
        se_y_lr<-sqrt(var_y_lr)
        inv_l<-y_lr-t*se_y_lr
        inv_u<-y_lr+t*se_y_lr
        cv_y_lr<-se_y_lr/y_lr
        result<-data.frame(y_lr,var_y_lr,se_y_lr,cv_y_lr,inv_l,inv_u)
}

#比率
ratio<-function(data,...) {
        x<-data[,1]
        y<-data[,2]
        n<-data[1,3]
        N<-data[1,4]
        X<-data[1,5]
        t<-data[1,6]
        f<-n/N
        R<-sum(y)/sum(x)
        y_r<-X*R
        var_y_r<-N^2*(1-f)/n*(sum(y^2)+R^2*sum(x^2)-2*R*sum(y*x))/(n-1)
        se_y_r<-sqrt(var_y_r)
        inv_l<-y_r-t*se_y_r
        inv_u<-y_r+t*se_y_r
        cv_y_r<-se_y_r/y_r
        result<-data.frame(y_r,var_y_r,se_y_r,cv_y_r,inv_l,inv_u)
}

#简单
srs<-function(data,...) {
        x<-data[,1]
        y<-data[,2]
        n<-data[1,3]
        N<-data[1,4]
        t<-data[1,6]
        f<-n/N
        y_mean<-mean(y)
        y_srs<-y_mean*N
        var_y_srs<-N^2*(1-f)*var(y)/n
        se_y_srs<-sqrt(var_y_srs)
        inv_l<-y_srs-t*se_y_srs
        inv_u<-y_srs+t*se_y_srs
        cv_srs<-se_y_srs/y_srs
        result<-data.frame(y_srs,var_y_srs,se_y_srs,cv_srs,inv_l,inv_u)
}

#展示
listing<-function(data,...) {
        cat("\n简单估计：\n")
        print(srs(data))
        
        cat("\n比率估计：\n")
        print(ratio(data))
        
        cat("\n回归估计：\n")
        print(lr(data))
        
        cat("\nPPS抽样：\n")
        print(hh(data))
        
        cat("\n各组设计效果比较 deff\n")
        
        cat("\npps与简单抽样 deff_hh&srs\n")
        deff_hh<-hh(data)[2]/srs(data)[2]
        print(deff_hh[1,1])
        
        cat("\n回归与简单抽样 deff_lr&srs\n")
        deff_lr<-lr(data)[2]/srs(data)[2]
        print(deff_lr[1,1])
        
        cat("\n比率与简单抽样 deff_ratio&srs\n")
        deff_ratio<-ratio(data)[2]/srs(data)[2]
        print(deff_ratio[1,1])
}
