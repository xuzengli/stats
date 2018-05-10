a=read.csv("课后练习1.csv")
cat("\n读入数据如下（前五行）：\n")
names(a)=c("Y","X1","X2","X3","X4","X5","X6","X7","X8","X9")
print(a[c(1:5),])
# 直方图
hist(a$Y)
# 删除cook距离大的点
# a <- a[c(-141,-342,-364,-389,-462,-482),]


N=sapply(a,length)
MU=sapply(a,mean)
SD=sapply(a,sd)
MIN=sapply(a,min)
MED=sapply(a,median)
MAX=sapply(a,max)
result=cbind(N,MU,SD,MIN,MED,MAX)
cat("\n描述分析如下：\n")
print(result)
cat("\n最小二乘估计：
Y  因变量.下一年净资产收益率
X1 资产周转率   X2 利润率
X3 债务资本比率 X4 成长速度
X5 市倍率       X6 收入质量
X7 存货率       X8 资产规模
X9 当年净资产收益率\n")
lm1=lm(Y~X1+X2+X3+X4+X5+X6+X7+X8+X9,data=a)
print(lm1)
cat("\n方差分析：\n")
print(anova(lm1))
cat("\n参数估计：\n")
print(summary(lm1))
# 模型诊断
par(mfrow=c(2,4))
plot(lm1,which=c(1:4))
plot(a$X3,a$Y,xlab="债务资本比率",ylab="下一年净资产收益率",main="Y vs X3")
plot(a$X5,a$Y,xlab="市倍率",ylab="下一年净资产收益率",main="Y vs X5")
plot(a$X9,a$Y,xlab="当年净资产收益率",ylab="下一年净资产收益率",main="Y vs X9")
