a=read.csv("�κ���ϰ1.csv")
cat("\n�����������£�ǰ���У���\n")
names(a)=c("Y","X1","X2","X3","X4","X5","X6","X7","X8","X9")
print(a[c(1:5),])
# ֱ��ͼ
hist(a$Y)
# ɾ��cook�����ĵ�
# a <- a[c(-141,-342,-364,-389,-462,-482),]


N=sapply(a,length)
MU=sapply(a,mean)
SD=sapply(a,sd)
MIN=sapply(a,min)
MED=sapply(a,median)
MAX=sapply(a,max)
result=cbind(N,MU,SD,MIN,MED,MAX)
cat("\n�����������£�\n")
print(result)
cat("\n��С���˹��ƣ�
Y  �����.��һ�꾻�ʲ�������
X1 �ʲ���ת��   X2 ������
X3 ծ���ʱ����� X4 �ɳ��ٶ�
X5 �б���       X6 ��������
X7 �����       X8 �ʲ���ģ
X9 ���꾻�ʲ�������\n")
lm1=lm(Y~X1+X2+X3+X4+X5+X6+X7+X8+X9,data=a)
print(lm1)
cat("\n���������\n")
print(anova(lm1))
cat("\n�������ƣ�\n")
print(summary(lm1))
# ģ�����
par(mfrow=c(2,4))
plot(lm1,which=c(1:4))
plot(a$X3,a$Y,xlab="ծ���ʱ�����",ylab="��һ�꾻�ʲ�������",main="Y vs X3")
plot(a$X5,a$Y,xlab="�б���",ylab="��һ�꾻�ʲ�������",main="Y vs X5")
plot(a$X9,a$Y,xlab="���꾻�ʲ�������",ylab="��һ�꾻�ʲ�������",main="Y vs X9")
