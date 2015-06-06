#数据表格式要求，变量名及顺序
#从左到右：time ph ntu antu take supply pac ntu_judge pac_judge
#从左到右：时间 ph 原水浊度 出水浊度 取水量 供水量 浊度判断 投药判断
#计算pac_judge:effect_real3(出水浊度,loop2(checkitout(数据表)))
#以下函数均适用

#输入当前操作的数据表
checkitout<-function(data,...) {
        #指标（单位药效）计算，浊度差*供水量/投药量
        check=(data$ntu-data$antu)*data$supply/data$pac/10000
        check
}

#输入要处理的列
judge<-function(data,...) {
        #判断出水浊度合格与否，小于1.10标为1，其余标为0
        index=numeric(length(data))
        for (i in 1:length(data)) {
                if (data[i]<=1.10) {
                        index[i]=1
                } else {index[i]=0}
        }
        index
}

#缺失值处理，输入要处理的列
deal_na<-function(d,...) {
        data=d
        row.na=which(is.na(data))
        len.na=length(row.na)
        len=length(data)
        if (row.na[len.na]==len) {
                #最后一个是缺失值，先用临近点均值法，再用线性插值
                array=1:len
                n=len.na-1
                for (i in 1:n) {
                        data[row.na[i]]=mean(c(data[row.na[i]-1],data[row.na[i]+1]))
                }
                d=data.frame(data,array)
                pdata=predict(lm(data~array),d[2])
                data[len]=pdata[len]
        } else {
                #最后一个不是缺失值，只用临近点均值
                array=1:len
                n=len.na
                for (i in 1:n) {
                        data[row.na[i]]=mean(c(data[row.na[i]-1],data[row.na[i]+1]))
                }
        }
        data
}

#扩充时间序列小时->分钟
expendtime<-function(data,t=60,...) {
        len=length(data)
        series=numeric((len-1)*t+1)
        for (i in 1:(len-1)) {
                start=data[i]
                end=data[i+1]
                split=abs(start-end)/t
                temp=0
                for (k in 1:(t+1)) {
                        if (start<end) {
                                temp[k]=start+(k-1)*split
                        } else {
                                temp[k]=start-(k-1)*split
                        }
                }
                for (j in 1:length(temp)) {
                        series[(i-1)*t+1+j-1]=temp[j]
                }
        }
        series
}

#pac_judge未判断是否合格，带缺失值
effect_real<-function(data,lag,...) {
        expend=expendtime(data,60)
        expend2=expend[(lag+1):length(expend)]
        series=0
        for (i in 1:length(data)) {
                series[i]=expend2[(i-1)*60+1]
        }
        series
}

#pac_judge未判断是否合格，不带缺失值
effect_real2<-function(data,lag,...) {
        expend=expendtime(data,60)
        expend2=expend[(lag+1):length(expend)]
        series=0
        for (i in 1:(length(data)-2)) {
                series[i]=expend2[(i-1)*60+1]
        }
        series=judge(series)
        series
}

#pac_judge已判断是否合格，带缺失值
effect_real3<-function(data,lag,...) {
        expend=expendtime(data,60)
        expend2=expend[(lag+1):length(expend)]
        series=0
        for (i in 1:length(data)) {
                series[i]=expend2[(i-1)*60+1]
        }
        for (j in 1:(length(series)-2)) {
                if (series[j]<=1.10) {
                        series[j]=1
                } else if (series[j]>1.10) {
                        series[j]=0
                }
        }
        series
}

#输入要处理的列
loop2<-function(original,...) {
        #计算时滞的函数
        N<-length(original)
        n<-numeric(length(original))
        j<-1
        for (i in 70:120) {
                temp=effect_real2(original,i)
                n[j]=sum(temp)/length(temp)
                j=j+1
                temp=0
        }
        timelag<-70+which.max(n)-1
        timelag
}

#计算pac_judge:effect_real3(出水浊度,loop2(checkitout(数据表)))
