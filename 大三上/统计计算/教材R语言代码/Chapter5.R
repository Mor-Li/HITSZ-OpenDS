#######################################################################
#  Chapter 5
####################################################################

###Ex5.1.1
set.seed(1)
n=1000
x=runif(n)
y=runif(n)
g=function(x) sin(x)/x
I=length(x[y<=g(x)])/n      #sum(y<=g(x))/n
plot(x,y,pch=16,col=c((y<=g(x))+1))
lines(sort(x),g(sort(x)),col="blue",lwd=2)


###Ex5.1.2
set.seed(1)
n=1000
x=runif(n)
I=sum(sin(x)/x)/n


###Ex5.2.1
n=100
hat_mu1=NULL
hat_mu2=NULL
for (i in 1:1000){
  data=runif(n,0,1)
  X=data
  hat_mu1[i]=mean(X)
  Y=(1-data)
  hat_mu2[i]=0.5*mean(X+Y)
}
c(mean(hat_mu1),mean(hat_mu2),var(hat_mu1),var(hat_mu2))


###Ex5.2.2
set.seed(1)
I= vector()
for(i in 1:1000){
  x = runif(100)
  I[i] = mean((sqrt(1-x^2)+sqrt(1-(1-x)^2))/2)
}
cat("E(I)",mean(I), "Var(I)", var(I))



###Ex5.2.3
n=20
hat_mu1=NULL
hat_mu2=NULL
for (i in 1:1000){
  U=runif(n,0,1)
  X=exp(U)
  Y=exp(1-U)
  hat_mu1[i]=mean(X)
  hat_mu2[i]=0.5*mean(X+Y)
}
c(mean(hat_mu1),mean(hat_mu2),var(hat_mu1),var(hat_mu2))



###Ex5.3.1
library(MASS)
n=100
hat_mu1=NULL
hat_mu2=NULL
Mu=c(5,0)
Sigma2=matrix(c(1,-0.8,-0.8,1),ncol=2,nrow=2,byrow=T)
for (i in 1:1000){
  Data=mvrnorm(n,Mu,Sigma2)
  X=Data[,1]
  hat_mu1[i]=mean(X);
  Y=Data[,2]
  hat_mu2[i]=mean(X+Y);
}
c(mean(hat_mu1),mean(hat_mu2),var(hat_mu1),var(hat_mu2))


###Ex5.3.2
set.seed(1)
n=100
hat_mu1=NULL
hat_mu2=NULL
for (i in 1:1000){
  U=runif(n,0,1)
  X=exp(U)
  hat_mu1[i]=mean(X)
  Y=1.690*(U-0.5)
  hat_mu2[i]=mean(X-Y)
}
c(mean(hat_mu1),mean(hat_mu2),var(hat_mu1),var(hat_mu2))


###Ex5.4.1
set.seed(1)
I1 = vector()
I2 = vector()
for(i in 1:1000){
  x = 2*runif(100)-1
  y = 2*runif(100)-1
  theta = (x^2+y^2<=1)
  I1[i] = mean(theta)
  I2[i] = mean(sqrt(1-x^2))
}
cat("E(I1)",mean(I1),"E(I2)",mean(I2))
cat('Var(I1)',var(I1),'Var(I2)',var(I2))


###Ex5.4.2
set.seed(1)
n = 100
I = NULL
for (i in 1:1000) {
  Y = rexp(n)
  Etheta = 1-pnorm(1-Y)
  I[i] = mean(Etheta)
}
cat('估计值',mean(I),'方差',var(I))


###Ex5.5.1
set.seed(1)
n = 100
I1 = NULL
I2 = NULL
Ginv = function(r) sqrt(1 + 3*r)-1
for (i in 1:1000){
  R = runif(n)
  Y = Ginv(runif(n))
  I1[i]= mean(exp(R))
  I2[i] = mean(3*exp(Y)/(2*(1 + Y)))
}
cat('平均值估计',mean(I1),'重要抽样估计',mean(I2))
cat('平均值估计方差',var(I1),'重要抽样估计方差',var(I2))


###Ex5.6.1
m<-4
n<-6
r1<-runif(m,min=0,manx=0.5)
r2<-runif(m,min=0.5,max=1)
I<-(mean(exp(r1))+mean(exp(r2)))/2
Sigma2<-var(exp(r1))/(4*m)+ var(exp(r2))/(4*n)


###Ex5.7.1
set.seed(1)
n=100
N=1000
I11=NULL
I21=NULL
I22=NULL
for (i in 1:N){
  X=rnorm(n,mean=0,sd=1)
  Y=rnorm(n,mean=0,sd=1)
  I11[i]=var(X)
  I21[i]=n*var(X)/(n-1)
  I22[i] =n*var(Y)/(n-1)
}
  #比较相同样本不同估计方法的精度
VDif1=var(I11-I21)
VDif2=var(I11-I22)
cat("E(I11)",mean(I11), "Var(I11)", var(I11))
cat("E(I21)",mean(I21), "Var(I21)", var(I21))
cat("E(I22)",mean(I22), "Var(I22)", var(I22))
cat("相同样本",VDif1, "不同样本", VDif2)