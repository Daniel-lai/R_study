library(DMwR)
library(nnet) #神经网络包
library(e1071) #支持向量机包
library(kknn) #k近邻包

#需求二线性模型
lm2<-lm(source6$pac~source6$ph+source6$ntu+source6$take)
r2_lm_2=summary(lm2)["r.squared"]
plot(lm2)
source6_2_lm<-cbind(source6,pac_lm_preds=predict(lm2,source6[,c(2,3,5)]))
write.csv(source6_2_lm,"source6_2_lm.csv",row.names=F)

#需求三线性模型
lm3<-lm(source6$pac~source6$ph+source6$ntu+source6$antu+source6$take)
r2_lm_3=summary(lm3)["r.squared"]
plot(lm3)
source6_3_lm<-cbind(source6,pac_lm_preds=predict(lm3,source6[,c(2,3,4,5)]))
write.csv(source6_3_lm,"source6_3_lm.csv",row.names=F)

#用于需求二建模的数据表
source6_2<-data.frame(ph=source6$ph,ntu=source6$ntu,take=source6$take,pac=source6$pac)
#用于需求三建模的数据表
source6_3<-data.frame(ph=source6$ph,ntu=source6$ntu,antu=source6$antu,take=source6$take,pac=source6$pac)

#需求二
#对source6_2进行标准化
norm.source6_2<-scale(source6_2)
mean_source6_2_pac<-mean(source6_2$pac)
sd_source6_2_pac<-sd(source6_2$pac)

#训练BP神经网络模型
nn.source6_2<-nnet(pac~.,norm.source6_2,size=10,decay=0.01,
                   maxit=10000,linout=T,trace=F)
#进行预测
norm.preds.source6_2.scale.nnet2<-predict(nn.source6_2,norm.source6_2)
#预测值标准化还原
pac_nnet_preds<-predict(nn.source6_2,norm.source6_2)*sd_source6_2_pac+mean_source6_2_pac
#带预测值的数据表
source6_2_nnet<-cbind(source6,pac_nnet_preds=pac_nnet_preds)
write.csv(source6_2_nnet,"source6_2_nnet.csv",row.names=F)
#作图
plot(norm.preds.source6_2.scale.nnet2~scale(source6_2$pac))
#R^2
r2_nnet_2=1-sum((source6$pac-pac_nnet_preds)^2)/sum((source6$pac-mean(source6$pac))^2)

#SVM建模
svm.source6_2<-svm(pac~.,norm.source6_2)
#预测值
preds.source6_2.scale.svm2<-predict(svm.source6_2,norm.source6_2)
#预测值标准化还原
pac_svm_preds<-predict(svm.source6_2,norm.source6_2)*sd_source6_2_pac+mean_source6_2_pac
#作图
plot(preds.source6_2.scale.svm2~scale(source6_2$pac))
#带预测值的数据表svm
source6_2_svm<-cbind(source6,pac_svm_preds=pac_svm_preds)
write.csv(source6_2_svm,"source6_2_svm.csv",row.names=F)
#R^2
r2_svm_2=1-sum((source6$pac-pac_svm_preds)^2)/sum((source6$pac-mean(source6$pac))^2)

#需求三
#对source6_3进行标准化
norm.source6_3<-scale(source6_3)
mean_source6_3_pac<-mean(source6_3$pac)
sd_source6_3_pac<-sd(source6_3$pac)

#训练BP神经网络模型
nn.source6_3<-nnet(pac~.,norm.source6_3,size=10,decay=0.01,
                   maxit=10000,linout=T,trace=F)
#进行预测
norm.preds.source6_3.scale.nnet3<-predict(nn.source6_3,norm.source6_3)
#预测值标准化还原
pac_nnet_preds<-predict(nn.source6_3,norm.source6_3)*sd_source6_3_pac+mean_source6_3_pac
#带预测值的数据表
source6_3_nnet<-cbind(source6,pac_nnet_preds=pac_nnet_preds)
write.csv(source6_3_nnet,"source6_3_nnet.csv",row.names=F)
#作图
plot(norm.preds.source6_3.scale.nnet3~scale(source6_3$pac))
#R^2
r2_nnet_3=1-sum((source6$pac-pac_nnet_preds)^2)/sum((source6$pac-mean(source6$pac))^2)

#SVM建模
svm.source6_3<-svm(pac~.,norm.source6_3)
#预测值
preds.source6_3.scale.svm3<-predict(svm.source6_3,norm.source6_3)
#预测值标准化还原
pac_svm_preds<-predict(svm.source6_3,norm.source6_3)*sd_source6_3_pac+mean_source6_3_pac
#作图
plot(preds.source6_3.scale.svm3~scale(source6_3$pac))
#带预测值的数据表svm
source6_3_svm<-cbind(source6,pac_svm_preds=pac_svm_preds)
write.csv(source6_3_svm,"source6_3_svm.csv",row.names=F)
#R^2
r2_svm_3=1-sum((source6$pac-pac_svm_preds)^2)/sum((source6$pac-mean(source6$pac))^2)

#k近邻模型
#需求二
knn.source6_2<-train.kknn(pac~.,source6_2,kmax=3,distance=1,kernel="optimal")
pac_knn_preds<-predict(knn.source6_2,source6_2[,1:(length(source6_2)-1)])
source6_2_knn<-cbind(source6,pac_knn_preds=pac_knn_preds)
#作图
plot(pac_knn_preds~source6_2$pac)
#标准差
sqrt(sum((source6_2_knn$pac_knn_preds-source6_2$pac)^2)/(nrow(source6_2_knn)-1))
#R^2
r2_knn_2=1-sum((source6_2_knn$pac_knn_preds-source6_2$pac)^2)/sum((source6_2_knn$pac-mean(source6_2$pac))^2)

#需求三
knn.source6_3<-train.kknn(pac~.,source6_3,kmax=3,kernel="optimal",distance=1)
pac_knn_preds3<-predict(knn.source6_3,source6_3[,1:(length(source6_3)-1)])
source6_3_knn<-cbind(source6,pac_knn_preds=pac_knn_preds3)
#作图
plot(pac_knn_preds3~source6_2$pac)
#R^2
r2_knn_3=1-sum((source6_3_knn$pac_knn_preds-source6_3$pac)^2)/sum((source6_3_knn$pac-mean(source6_3$pac))^2)

Summary=data.frame(lm=c(as.numeric(r2_lm_2),as.numeric(r2_lm_3)),nnet=c(r2_nnet_2,r2_nnet_3),
                   svm=c(r2_svm_2,r2_svm_3),knn=c(r2_knn_2,r2_knn_3),row.names=c(2,3))
Summary
#Summary为各模型各需求的R^2
write.csv(Summary,"Summary.csv",row.names=F)

#需求二里加入模型knn.source6_2预测的PAC投药量后的数据表
source_2<-data.frame(source,pac_preds=predict(knn.source6_2,source[,c(2,3,6)]))
#需求三里加入模型knn.source6_3预测的PAC投药量后的数据表
source_3<-data.frame(source2,pac_preds=predict(knn.source6_3,source3[,2:5]))

#预览需求二里加入模型knn.source6_2预测的PAC投药量后的数据表
tail(source_2)
#预览需求三里加入模型knn.source6_3预测的PAC投药量后的数据表
head(source_3)
