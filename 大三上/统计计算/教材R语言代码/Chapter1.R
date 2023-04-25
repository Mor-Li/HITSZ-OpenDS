#######################################################################
#  Chapter 1
####################################################################

###Ex1.1.1

set.seed(1)
n = 10000 #确定产生随机数的个数
sigma=1 #确定sigma
r = matrix(runif(n),n,1) #产生[0,1]上的均匀分布随机数
x =sqrt(-2*sigma^2*log(1-r)) #代入抽样公式
x0=seq(0,4,0.01)
fx=x0/(sigma^2)*exp(-x0^2/(2*sigma^2)) #概率密度函数
hist(x,20,freq=F) #查看随机数分布情况
lines(x0,fx,'l') #查看频率直方图和概率密度函数是否吻合

###Ex1.1.2

set.seed(1);
n = 10000 #确定产生随机数的个数
r = matrix(runif(n),n,1) #产生[0,1]上的均匀分布随机数
x =(-1+sqrt((1+8*r)))/2 #代入抽样公式
x0=seq(0,1,0.001);
fx=x0+0.5 #概率密度函数
hist(x,20,freq=F) #查看随机数分布情况
lines(x0,fx,'l') #查看频率直方图和概率密度函数是否吻合
index=((matrix(1,length(x0),1)%*%t(x))<=x0%*%matrix(1,1,n))
Fx=rowMeans(index) #计算累积频率
Fx0=(x0^2+x0)/2 #理论分布函数
#查看累积频率和理论分布函数是否吻合
plot(x0,Fx,'l');
lines(x0,Fx0,'l',col='red')



###Ex1.1.3

set.seed(1)
func_h = function(x,alpha, beta){ #编写h(x)函数
  if(alpha == 1 || beta == 1){
    print("ERROR_PARAMETER") #alpha=1或beta=1时该抽样不适用
  }
  else ((alpha+beta-2)^(alpha+beta-2)*(x/(alpha-1))
        ^(alpha-1)*((1-x)/(beta-1))^(beta-1))}
n = 1000 #确定产生随机数的个数
m = 0 #记录产生的舍选随机数个数
i = 0;
alpha = 3;
beta = 2 #确定参数数值
z = c()
repeat{
  m = m+1;x = runif(1)
  y = runif(1) #生成[0,1]上的均匀分布随机数
  ay = func_h(x,alpha,beta) #计算舍选条件
  if (y<=ay){ #进行舍选检验
    z = c(z,x)
    i = i+1}
  if (i == n) break}
x0=seq(0,1,0.001)
fx=dbeta(x0,alpha,beta) #概率密度函数		
hist(z,30,freq=F)
#查看频率直方图和概率密度函数是否吻合
lines(x0,fx,'l',col='red')


###Ex1.1.4

set.seed(1)
n = 10000 #确定产生随机数的个数
m = 0 #记录产生的舍选随机数个数
i = 0; 
z = c()
repeat{
  m = m+1; 
  x = rexp(1,1)
  y = runif(1) #生成[0,1]上的均匀分布随机数
  ay = exp(-(x-1)^2/2) #计算舍选条件
  if (y<=ay){ #进行舍选检验
    z = c(z,x);i = i+1}
  if (i == n) break}
x0=seq(0,max(z),0.01)
fx=sqrt(2/pi)*exp(-x0^2/2) #概率密度函数
hist(z,20,freq=F);lines(x0,fx,'l',col='red')
#查看频率直方图和概率密度函数是否吻合


###Ex1.1.5
set.seed(1)
func_h = function(x,alpha){ #编写h(x)函数
  if(alpha < 1 ){
    print("ERROR_PARAMETER") #当alpha<1时该抽样不适用
  }
  else (exp(alpha-1)/(alpha^(alpha-1))*x^(alpha-1)
        *exp(-(alpha-1)/alpha*x))}
n = 1000 #确定产生随机数的个数
m = 0 #记录产生的舍选随机数个数
i = 0;alpha = 3 #确定参数数值
z = c()
repeat{
  m = m+1;
  x = rexp(1,1/alpha)
  y = runif(1) #生成[0,1]上的均匀分布随机数
  ay = func_h(x,alpha) #计算舍选条件
  if (y<=ay){ #进行舍选检验
    z = c(z,x)
    i = i+1}
  if (i == n) break}
x0=seq(0,max(z),0.01)
fx=dgamma(x0,alpha,1) #概率密度函数
hist(z,30,freq=F)
#查看频率直方图和概率密度函数是否吻合
lines(x0,fx,'l',col='red')


###Ex1.1.6
set.seed(1)
n = 10000 #确定产生随机数的个数
mu=0.6
sigma=1.5
xi = rnorm(n,0,1)
eta = mu + sigma*xi

#查看随机数分布情况
hist(eta,30,freq=F)
#查看频率直方图和概率密度函数是否吻合
x0=seq(min(eta),max(eta),0.01)
lines(x0,dnorm(x0,mu,sigma),'l',col='red')


###Ex1.1.7
set.seed(1)
n = 100000 #确定产生随机数的个数
alpha=1/2
gamma=3
a=gamma/0.5
zeta = rnorm(n,0,1)
xi = zeta^2
eta=xi/a
#查看随机数分布情况
hist(eta,30,freq=F);x0=seq(min(eta),max(eta),0.01)
#查看频率直方图和概率密度函数是否吻合
lines(x0,dgamma(x0,alpha,gamma),'l',col='red')

###Ex1.1.8
set.seed(1)
n = 10000 #确定产生随机数的个数
m=4 #确定卡方分布的自由度（产生标准正态随机数的个数）
eta=NULL
for (i in 1:n){
  x=rnorm(m,0,1)
  eta[i]=sum(x^2) #平方和为自由度为m的卡方分布的随机数
}
hist(eta,30,freq=F)
#查看频率直方图和概率密度函数是否吻合
x0=seq(min(eta),max(eta),0.01)
lines(x0,dchisq(x0,m),'l',col='red')

###Ex1.1.9
set.seed(1)
n = 10000 #确定产生随机数的个数
a=5;b=10  #确定参数a,b
m=a+b-1 #确定均匀随机数个数
eta=NULL
for (i in 1:n){
  x=runif(m,0,1);sx=sort(x) #将x排序
  eta[i]=sx[a] #第a个次序统计量为Beta(a,b)的随机数
}
hist(eta,30,freq=F);x0=seq(min(eta),max(eta),0.01)
#查看频率直方图和概率密度函数是否吻合
lines(x0,dbeta(x0,a,b),'l',col='red')

###Ex1.1.10

set.seed(1)
n = 10000;eta=NULL #确定产生随机数的个数
for (i in 1:n){
  u=runif(1,0,1)
  if (u<=0.5){
    eta[i]=rexp(1,1)}else{
      R=runif(1,0,1);eta[i]=log(R)
    }}
hist(eta,30,freq=F) #查看随机数分布情况
#查看频率直方图和概率密度函数是否吻合
x0=seq(min(eta),max(eta),0.01)
lines(x0,0.5*exp(-abs(x0)),'l',col='red')


###Ex1.1.11

set.seed(1)
n = 10000;eta=NULL  #确定产生随机数的个数
mu1=0; mu2=1; mu3=-2
sigma1=1; sigma2=1.5; sigma3=2
for (i in 1:n){
  u=runif(1,0,1)
  if (u<=1/3){
    eta[i]=rnorm(1,mu1,sigma1)
  }else if (u>1/3&u<=2/3){
    eta[i]=rnorm(1,mu2,sigma2)
  }else {
    eta[i]=rnorm(1,mu3,sigma3)
  }}
hist(eta,30,freq=F)  #查看随机数分布情况
#查看频率直方图和概率密度函数是否吻合
x0=seq(min(eta),max(eta),0.01)
lines(x0,1/3*(dnorm(x0,mu1,sigma1)+dnorm(x0,mu2,sigma2)
              +dnorm(x0,mu3,sigma3)),'l',col='red')

###Ex1.1.12
set.seed(1)
n = 10000 #确定产生随机数的个数
eta=NULL
m=12
for (i in 1:n){
  u=runif(m,0,1)
  eta[i]=sqrt(12/m)*sum(u-1/2)
}
hist(eta,30,freq=F)
#查看频率直方图和概率密度函数是否吻合
x0=seq(min(eta),max(eta),0.01)
lines(x0,dnorm(x0,0,1),'l',col='red')


###Ex1.1.13

set.seed(1)
n = 100000 #确定产生随机数的个数
eta=NULL;m=40 #确定小区间个数
Npdf=NULL;Ncdf=NULL
fx=function(x){
  resu=sqrt(1/(2*pi))*exp(-x^2/2) #理论概率密度函数
  return(resu)	}
Fx=function(x){
  resu=integrate(fx,lower=-6,upper=x)$value #理论分布函数
  return(resu)
}

x=seq(-6,6,0.0001);Npdf=fx(x)
for (i in 1:length(x)){
  Ncdf[i]=Fx(x[i])}
xd=NULL
for (i in 1:m){ #确定m个小区间的分割点
  index=min(which(abs(Ncdf-i/m)<0.0001))
  xd[i]=x[index]}
xd=c(x[1],xd)
for (i in 1:n){
  r=runif(1,0,1);k=floor(m*r+1)
  u1=runif(1,0,1); u2=runif(1,0,1)
  dk=abs(fx(xd[k+1])-fx(xd[k]))/(fx(xd[k+1])+fx(x[k]))
  if (u2<=dk){
    if (fx(xd[k+1])>fx(xd[k])){
      eta[i]=xd[k]+(xd[k+1]-xd[k])*sqrt(u1)
    }else{
      eta[i]=xd[k+1]-(xd[k+1]-xd[k])*sqrt(1-u1)
    }
  }else{
    eta[i]=xd[k]+(xd[k+1]-xd[k])*u1
  }}
#查看频率直方图和概率密度函数是否吻合
hist(eta,40,xlim=c(-6,6),freq=F)
lines(x,dnorm(x,0,1),'l',col='red')


###Ex1.1.14
set.seed(1)
n = 100000 #确定产生随机数的个数
eta=NULL
m=40 #确定小区间个数
p=NULL
Fx=function(x){
  resu=integrate(fx,lower=-6,upper=x)$value #理论分布函数
  return(resu)
}
x=seq(-6,6,0.0001)
xd=x[1]+seq(0,m)*12/m #平均分割取值区间
for (i in 1:(length(xd)-1)){
  p[i]=Fx(xd[i+1])-Fx(xd[i]) #计算权系数
}
cump=cumsum(p)
for (i in 1:n){
  r=runif(1,0,1)
  k=min(which(cump>=r)) #确定抽样区间为第k个区间
  u=runif(1,0,1)
  eta[i]=xd[k]+(xd[k+1]-xd[k])*u
}
#查看频率直方图和概率密度函数是否吻合
hist(eta,40,xlim=c(-6,6),freq=F)
lines(x,dnorm(x,0,1),'l',col='red')

########################################################################
#1.2
###Ex1.2.1

set.seed(1);X=c(1,2.5,3.5,5,6);
prob = c(0.1,0.2,0.3,0.2,0.2) #确定离散分布律
Fx=cumsum(prob) #确定分布函数
n = 10000;xi=NULL #确定产生随机数的个数
for (i in 1:n){
  r=runif(1,0,1)
  if (r<=prob[1]){
    xi[i]=X[1]
  }else{xi[i]=X[min(which(Fx>=r))]}}
x=sample(c(1,2.5,3.5,5,6),size=n,replace=TRUE,prob=prob)
table(xi)/n #查看逆变换法产生的随机数分布情况
table(x)/n #查看直接抽样产生的随机数分布情况