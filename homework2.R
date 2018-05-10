# ��������
a=read.csv("�κ���ϰ2.csv")
names(a)=c("Y","X1","X2","X3","X4","X5")

a$X4=factor(a$X4, levels=c("��ר","����","˶ʿ","˶ʿ������"))
# �ɼ�ȡ����
a$Y <- log(a$Y)
# ���������е����
a$X2 <- sapply(a$X2,function(x){paste(substring(x,1,3),"0���",sep="")})
a$X5 <- sapply(a$X5,function(x){paste(substring(x,1,3),"0���",sep="")})
a[c(1:5),]

par(mfrow=c(2,3))
hist(a$Y,main="ƽ���ɼ���Y���ֲ�ͼ",xlab="ƽ���ɼ���������",ylab="Ƶ��")
boxplot(Y~X1,a,ylab="�ɼ�")
boxplot(Y~X2,a,xlab="�������",ylab="�ɼ�")
boxplot(Y~X3,a,ylab="�ɼ�")
boxplot(Y~X4,a,ylab="�ɼ�")
boxplot(Y~X5,a,xlab="��ҵ���",ylab="�ɼ�")

# ������������
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
  
# ���Ա�����������������
analyze(a$X1)
# ��������ݶ����������������
analyze(a$X2)
# ����ҵ���ʶ����������������
analyze(a$X3)
# �����ѧ�������������������
analyze(a$X4)
# ����ҵ��ݶ����������������
analyze(a$X5)

#���������
lm1=lm(Y~X1+X2+X3+X4+X5,data=a)
anova(lm1)

#ȥ��X3��X5�ķ��������
lm2=lm(Y~X1+X2+X4,data=a)
anova(lm2)
summary(lm2)
