#简单估计
srs<-function(...) { #simple random sampling
        y_srs<-y_mean*N
        var_y_srs<-N^2*(1-f)*var(y)/n
        se_y_srs<-sqrt(var_y_srs)
        interval_l<-y_srs-t*se_y_srs
        interval_u<-y_srs+t*se_y_srs
        result<-data.frame(y_srs,var_y_srs,se_y_srs,interval_l,interval_u)
        print("简单估计：")
        print(result)
}

#比率估计
ratio<-function(...) {
        R<-sum(y)/sum(x)
        y_r<-X*R
        var_y_r<-N^2*(1-f)/n*(sum(y^2)+R^2*sum(x^2)-2*R*sum(y*x))/(n-1)
        se_y_r<-sqrt(var_y_r)
        interval_l<-y_r-t*se_y_r
        interval_u<-y_r+t*se_y_r
        result<-data.frame(y_r,var_y_r,se_y_r,interval_l,interval_u)
        print("比率估计：")
        print(result)
}

#回归估计
lr<-function(...) {
        b<-cov(y,x)/cov(x,x)
        y_lr<-N*(y_mean-b*(x_mean-X_mean))
        var_y_lr<-N^2*(1-f)/n*sum(((y-y_mean)-b*(x-x_mean))^2)/(n-2)
        se_y_lr<-sqrt(var_y_lr)
        interval_l<-y_lr-t*se_y_lr
        interval_u<-y_lr+t*se_y_lr
        result<-data.frame(y_lr,var_y_lr,se_y_lr,interval_l,interval_u)
        print("回归估计：")
        print(result)
}

#PPS抽样
hh<-function(data,...) {
        m<-data[,1]
        y<-data[,2]
        n<-data[1,3]
        M0<-data[1,4]
        t<-data[1,5]
        y_hh<-M0/n*sum(y/m)
        var_y_hh<-M0^2/n*sum((y/m-y_hh/M0)^2)/(n-1)
        se_y_hh<-sqrt(var_y_hh)
        interval_l<-y_hh-t*se_y_hh
        interval_u<-y_hh+t*se_y_hh
        result<-data.frame(y_hh,var_y_hh,se_y_hh,interval_l,interval_u)
        print("PPS抽样：")
        print(result)
}

#列表
listing<-function(...) {
        srs()
        ratio()
        lr()
        hh()
}