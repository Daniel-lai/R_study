library(lattice)
library(Rcpp)
library(mice)
library(grid)
#调用个人编写的函数
source("func.R")

#数据处理部分
source<-read.csv("source.csv") #导入数据
tail(source[source$pac=="NULL",]) #观察最后一行NULL
nrow(source[source$pac=="NULL",]) #计算NULL的行数

#source2
source2<-source[289:length(source$pac),] #删除前288行
write.csv(source2,"source2.csv",row.names=F)
#由于出水浊度取3号和4号池的平均值，合并3ntu和4ntu为antu作为出水浊度一列

#source3
source3<-data.frame(time=source2$time,ph=source2$ph,ntu=source2$ntu,
                    antu=(source2$ntu3+source2$ntu4)/2,
                    take=source2$take,supply=source2$supply,
                    pac=as.numeric(as.character(source2$pac)))
sum(complete.cases(source3)) #检测缺失值
summary(source3) #总览source3
write.csv(source3,"source3.csv",row.names=F)

#copy3
copy3<-source3
which(copy3$take<0) #检查取水
which(copy3$supply<0) #检查供水
which(copy3$pac==0) #检查投药
index_take<-which(copy3$take<0) #寄存有问题的取水记录
index_pac<-which(copy3$pac==0) #寄存有问题的投药记录
copy3[index_pac,7]=NA #把pac里为零的值记为缺失值
copy3[index_take,5]=NA #把take里为负的值记为缺失值
md.pattern(copy3) #表格右下角看出有八个缺失值，异常值变为缺失值成功
summary(copy3)
write.csv(copy3,"copy3.csv",row.names=F)

#copy3_1
#对处理过的copy3进行处理, 邻近点均值法+线性插值
copy3_1<-copy3
copy3_1=data.frame(time=copy3$time,ph=copy3$ph,ntu=copy3$ntu,
                   antu=copy3$antu,take=deal_na(copy3$take),
                   supply=copy3$supply,pac=deal_na(copy3_1$pac))
tail(copy3_1)
write.csv(copy3_1,"copy3_1.csv",row.names=F)
summary(copy3_1)

#source4
#生成瞬时浊度合格判断
source4<-cbind(copy3_1,ntu_judge=judge(copy3_1$antu))
write.csv(source4,"source4.csv",row.names=F)

#滞后时间求取
check=checkitout(source4) #每条记录的单位药效（浊度差*供水量/投药量）
timelag=loop2(checkitout(source4)) #滞后时间timelag
#source5
source5<-cbind(source4,pac_judge=effect_real3(source4$antu,timelag))
source5<-source5[!is.na(source5$pac_judge),]
write.csv(source5,"source5.csv",row.names=F)

#source6
#筛选出投药合格的记录用于建模
source6<-source5[source5$pac_judge==1,1:7]
write.csv(source6,"source6.csv",row.names=F)
