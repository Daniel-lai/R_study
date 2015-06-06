#install.packages("mice")
#install.packages("DMwR")
#install.packages("nnet")
#install.packages("e1071")
#install.packages("kknn")
#上述包应首先运行安装
#run
#run all of functions
source("func.R")
#数据处理及时间滞后时间的计算
source("part1.R")

#计算到的时间为timelag
#最后用于建模的文件source6

#建模部分
source("part2.R")
#最后的表Summary会输出几个模型对于需求二三的拟合程度
#最后选到的模型为source6_2_knn和source6_3_knn对应需求二三