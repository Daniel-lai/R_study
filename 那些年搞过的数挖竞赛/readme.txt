切换工作目录到”run.R”的所在文件夹，输入命令source(“run.R”)即可。注，source.csv,func.R,part1.R,part2.R,run.R必须同在一个文件夹中。
应首先在R下自己运行安装：
install.packages("mice")
install.packages("DMwR")
install.packages("nnet")
install.packages("e1071")
install.packages("kknn")
install.packages("Rcpp")
install.packages("lattice")
install.packages("grid")

run的内容

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