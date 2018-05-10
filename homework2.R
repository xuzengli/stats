# 读入数据
a=read.csv("课后练习2.csv")
names(a)=c("Y","X1","X2","X3","X4","X5")

a$X4=factor(a$X4, levels=c("大专","本科","硕士","硕士或以上"))
# 成绩取对数
a$Y <- log(a$Y)
# 保留日期中的年代
a$X2 <- sapply(a$X2,function(x){paste(substring(x,1,3),"0年代",sep="")})
a$X5 <- sapply(a$X5,function(x){paste(substring(x,1,3),"0年代",sep="")})
a[c(1:5),]

par(mfrow=c(2,3))
hist(a$Y,main="平均成绩（Y）分布图",xlab="平均成绩（对数）",ylab="频数")
boxplot(Y~X1,a,ylab="成绩")
boxplot(Y~X2,a,xlab="出生年代",ylab="成绩")
boxplot(Y~X3,a,ylab="成绩")
boxplot(Y~X4,a,ylab="成绩")
boxplot(Y~X5,a,xlab="毕业年代",ylab="成绩")

# 描述分析函数
analyze <- function(aXN) {
  N=tapply(a$Y,aXN,length)
  MU=tapply(a$Y,aXN,mean)  
  SD=tapply(a$Y,aXN,sd)  
  MIN=tapply(a$Y,aXN,min)  
  MED=tapply(a$Y,aXN,median)  
  MAX=tapply(a$Y,aXN,max)  
  result=cbind(N,MU,SD,MIN,MED,MAX)
  result
}
  
# 按性别对因变量做描述分析
analyze(a$X1)
# 按出生年份对因变量做描述分析
analyze(a$X2)
# 按企业性质对因变量做描述分析
analyze(a$X3)
# 按最高学历对因变量做描述分析
analyze(a$X4)
# 按毕业年份对因变量做描述分析
analyze(a$X5)

#方差分析：
lm1=lm(Y~X1+X2+X3+X4+X5,data=a)
anova(lm1)

#去除X3、X5的分析结果：
lm2=lm(Y~X1+X2+X4,data=a)
anova(lm2)
summary(lm2)

