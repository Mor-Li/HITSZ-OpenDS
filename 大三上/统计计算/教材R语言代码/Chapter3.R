#######################################################################
#  Chapter 3
####################################################################

###Ex3.1.1
set.seed(1)
K=1000 #循环次数
n=500 #样本量
mu=0
sigma=1
resu=matrix(0,K,1)
for (i in 1:K){
data=rnorm(n,0,1)
resu[i]=mean(data) #第i组样本计算的均值估计
}
c(mean(resu),mu) #估计的bar X 的均值和参数真值
c(var(resu),sigma/n) #估计的bar X 的方差和理论方差


###Ex3.1.2
#选择~$n=50$，$K=1000$
set.seed(1)
library(MASS)
K=1000
n=50
theta1=matrix(0,K,1)
theta2=matrix(0,K,1)
for (i in 1:K){
data=mvrnorm(n,c(0,0),diag(2))
theta1[i]=mean(abs(data[,1]-data[,2]))
theta2[i]=var(abs(data[,1]-data[,2]))
}
c(mean(theta1),2/(sqrt(pi)))    #估计的|X1-X2|的均值和参数真值
c(mean(theta2),2-4/pi)          #估计的|X1-X2|的方差和参数真值


###Ex3.1.3
  #选择样本量n = 50，K = 1000
set.seed(1)
library(truncnorm)         #加载关于截尾正态分布的R包
K=1000
n=50
mu=0
sigma=1
a=0
b=1
thetahat=matrix(0,K,1)
theta=(dnorm(0,0,1)-dnorm(1,0,1))/(pnorm(1,0,1)-0.5)
for (i in 1:K){
data=rtruncnorm(n,0,1,0,1)
thetahat[i]=mean(data)             #第i组样本的均值
}
MSE=mean((thetahat-theta)^2)      #计算MSE


###Ex3.2.1
 # (1) sigma 已知，mu的置信区间
     #n=50, mu=0, sigma^2=1
set.seed(1)
K=1000                    #循环次数
n=500                      #样本量
a=0.05
mu=0
sigma=1
inter=matrix(0,K,2)
prob=matrix(0,K,1)
for (i in 1:K){
data=rnorm(n,mu,sigma)
barX=mean(data)
u=qnorm(1-a/2,mean=0,sd=1)/sqrt(n)
inter[i,]=c(barX-u,barX+u)
prob[i]=(mu>inter[i,1])&(mu<inter[i,2])
}
colMeans(inter)                    #区间估计
mean(inter[,2]-inter[,1])        # 平均长度
mean(prob)                         #覆盖率


 #（2）sigma未知，mu的置信区间
           #n=50，mu=0, sigma^2=1 
set.seed(1)
K=1000              #循环次数
n=50                   #样本量
a=0.05                #置信水平
mu=0                 #期望
sigma=1              #标准差
inter=matrix(0,K,2)
prob=matrix(0,K,1)
for(i in 1:K){
data=rnorm(n,mu,sigma)
barX=mean(data)
u=qt(1-a/2,n-1)*sd(data)/sqrt(n)
inter[i,]=c(barX-u,barX+u)
prob[i]=(mu>inter[i,1])&(mu<inter[i,2])
}
colMeans(inter)           #区间估计
mean(inter[,2]-inter[,1]) #平均长度
mean(prob)                 #覆盖率




###Ex3.2.2  总体方差的置信区间
     #n=50，(mu, sigma^2)=(0, 1)
set.seed(1)
K=1000      #循环次数
n=50          #样本量
a=0.05
mu=0
sigma=1
inter=matrix(0,K,2)
prob=matrix(0,K,1)
for(i in 1:K){
data=rnorm(n,mu,sigma)
Q=var(data)*(n-1)
chi1=qchisq(1-a/2,n-1)
chi2=qchisq(a/2,n-1)
inter[i,]=c(Q/chi1,Q/chi2)
prob[i]=(sigma>inter[i,1])&(sigma<inter[i,2])
}
colMeans(inter)
mean(inter[,2]-inter[,1])                #平均长度
mean(prob)



###Ex3.2.3  总体比例p的置信区间
  #n=50，p=0.2，置信水平alpha=0.05，重复次数1000
set.seed(1)
K=1000                #循环次数
n=50
a=0.05
p=0.2
inter=matrix(0,K,2)
prob=matrix(0,K,1)
for(i in 1:K){
data=rbinom(n,1,0.2)
barX=mean(data)
S=sqrt(barX*(1-barX)/n)
u=qnorm(1-a/2,mean=0,sd=1)*S
inter[i,]=c(barX-u,barX+u)
prob[i]=(p>inter[i,1])&(p<inter[i,2])
}
colMeans(inter)
mean(inter[,2]-inter[,1])            #平均长度
mean(prob)


###Ex3.2.4
  #（1）方差已知 两总体均值之差的置信区间
  #n=m=50，(mu1, sigma1)=(0, 1), (mu2, sigma2)=(0.2, 1)，alpha=0.05
set.seed(1)
K=1000               #循环次数
n=50
m=50
a=0.05
mu1=0
mu2=0.2
sigma1=1
sigma2=2
inter=matrix(0,K,2)
prob=matrix(0,K,1)
for(i in 1:K){
data1=rnorm(n,mu1,sigma1)
data2=rnorm(m,mu2,sigma2)
diff=mean(data1)-mean(data2)        #均值差
u=qnorm(1-a/2,mean=0,sd=1)*sqrt(sigma1^2/n+sigma2^2/m)
inter[i,]=c(diff-u,diff+u)
prob[i]=((mu1-mu2)>inter[i,1])&((mu1-mu2)<inter[i,2])
}
colMeans(inter);mean(prob)
mean(inter[,2]-inter[,1])                    #平均长度

  # （2）方差未知，两总体均值之差置信区间
      #n=m=50,(mu1, sigma1)=(0, 1),(mu2, sigma2)=(0.2, 1)
set.seed(1)
K=1000               #循环次数
n=50                #x样本量
m=50                #y样本量
a=0.05
mu1=0
mu2=0.2
sigma=1
inter=matrix(0,K,2)
prob=matrix(0,K,1)
for(i in 1:K){
data1=rnorm(n,mu1,sigma)
data2=rnorm(m,mu2,sigma)
diff=mean(data1)-mean(data2)    #均值差
S=(var(data1)*(n-1)+var(data2)*(m-1))/(n+m-2)
u=qt(1-a/2,n+m-2)*sqrt(S/n+S/m)
inter[i,]=c(diff-u,diff+u)
prob[i]=((mu1-mu2)>inter[i,1])&((mu1-mu2)<inter[i,2])
}
colMeans(inter)
mean(prob)
mean(inter[,2]-inter[,1])             #平均长度
mean(prob)


###Ex3.2.5  两个总体方差比的区间估计
   #n=m=50,(mu1, sigma1)=(0, 1),(mu2, sigma2)=(0.2, 2)
set.seed(1)
K=1000        #循环次数
n=50            #x样本量
m=50          #y样本量
a=0.05
mu1=0
mu2=0.2
sigma1=1
sigma2=2
inter=matrix(0,K,2);prob=matrix(0,K,1)
for(i in 1:K){
data1=rnorm(n,mu1,sigma1)
data2=rnorm(m,mu2,sigma2)
Sx=var(data1)
Sy=var(data2)
inter[i,]=c((Sx/Sy)/qf(1-a/2,n-1,m-1),
(Sx/Sy)/qf(a/2,n-1,m-1))
prob[i]=((sigma1^2/sigma2^2)>inter[i,1])&
((sigma1^2/sigma2^2)<inter[i,2])
}
colMeans(inter)
mean(prob)
mean(inter[,2]-inter[,1])            #平均长度


###Ex3.2.6  两个总体比例之差的区间估计
  #n=m=50,p1=0.2,p2=0.5
set.seed(1)
K=1000              #循环次数
n=m=500           #x和y的样本量
p1=0.2
p2=0.5
a=0.05
inter=matrix(0,K,2)
prob=matrix(0,K,1)
for (i in 1:K){
data1=rbinom(n,1,p1)
data2=rbinom(m,1,p2)
u=mean(data1)-mean(data2)            #均值差
S=sqrt(mean(data1)*(1-mean(data1))/n+mean(data2)*(1-mean(data2))/m)
inter[i,]=c(u-qnorm(1-a/2,mean=0,sd=1)*S,u+qnorm(1-a/2,mean=0,sd=1)*S)
prob[i]=((p1-p2)>inter[i,1])&((p1-p2)<inter[i,2])
}
colMeans(inter)
mean(prob)
mean(inter[,2]-inter[,1]) #平均长度
