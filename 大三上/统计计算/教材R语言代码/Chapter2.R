#######################################################################
#  Chapter 2
####################################################################

###Ex2.1.1
set.seed(1)
n=1000
lambda=1
eta=matrix(0,n,2)
xi1=rexp(n,lambda)
xi2=rexp(n,lambda)
eta1=xi1-xi2
eta2=xi2
eta=cbind(eta1,eta2)


###Ex2.1.2
set.seed(1)
n=1000
mu_1=mu_2=0
sigma_1=sigma_2=2
rho=0.8
xi1=rnorm(n,0,1)
xi2=rnorm(n,0,1)
eta1=sigma_1*xi1+mu_1
eta2=(sqrt(1-rho^2)*xi2+rho*xi1)*sigma_2+mu_2
c(mean(eta1),mean(eta2))
matrix(c(var(eta1),cor(eta1,eta2),cor(eta1,eta2),var(eta1)),2)


###Ex2.1.3
set.seed(1)
p=5
generateVector = function(p){
  vec = c()
  for (i in 1:p){
    point = 1 / (sum(p*(vec^(p-1))) + p-i+1)
    threshold = runif(1)
    if (threshold < point){
      vec = c(vec, (runif(1)^(1/p)))
    }else{
      vec = c(vec, (runif(1)))
    }
  }
  return(vec)
}
xi=generateVector(p)


###Ex2.1.4
set.seed(1)
rep=1000
xi=matrix(0,rep,2)
mu1=mu2=0
sigma11=1
sigma22=0.2
rho=0.8
sigma12=sigma21=rho*sqrt(sigma11*sigma22)
sigma_c=sigma11-sigma12*sigma22^(-1)*sigma21
for (i in 1:rep){
  x2=rnorm(1,mu2,sigma22^0.5)
  mu_c=mu1+sigma12*sigma22^(-1)*(x2-mu2)
  x1=rnorm(1,mu_c,sigma_c^0.5)
  xi[i,]=c(x1,x2)
}


###Ex2.1.5
set.seed(1)
n=1000
XY = c()
for (i in 1:n) {
  r = 2
  while(r>=1){
    u1 = runif(1)
    u2 = runif(1)
    X1 = 2*u1-1;
    Y1 = 2*u2-1
    r = X1^2 + Y1^2
  }
  XY = rbind(XY,c(X1,Y1))
}


###Ex2.1.6
set.seed(1)
p=5
point = 0
f0 = p
while(point == 0){
  r0 = runif(1)
  r = runif(p)
  f = 0
  for(i in 1:p){
    f = f + r[i]^(p-1)
  }
  if(f0*r0<f){point = 1}
}
x = r


###Ex2.2.1
set.seed(1)
X=c(0,1,2,3)
prob_x = choose(3,X)*(1/3)^X*(2/3)^(3-X) #确定离散分布律
Fx=cumsum(prob_x) #确定分布函数
n = 100 #确定产生随机数的个数
data=matrix(0,n,2)
for (i in 1:n){
  r1=runif(1,0,1)
  if (r1<=prob_x[1]){
    x=X[1]
  }else{
    x=X[min(which(Fx>=r1))]
  }
  Y=seq(0,3-x,1)
  prob_cy=choose(3-x,Y)*(1/2)^Y*(1/2)^(3-x-Y)
  Fcy=cumsum(prob_cy) #确定分布函数
  r2=runif(1,0,1)
  if (r2<=prob_cy[1]){ y=Y[1] }
  else{  y=Y[min(which(Fcy>=r2))] }
  data[i,]=matrix(c(x,y),1,2)
}
table(data[,1])/n
table(data[,2])/n


###Ex2.2.2
set.seed(1)
n=50
r = 4
pi = c(0.1,0.2,0.3,0.4)
x = c()
tx = rep(0,r)
for(t in 1:1000){
  tx = sum(rbinom(n,1,pi[1]))
  for(i in 2:r){
    tx[i] = sum(rbinom(n-sum(tx[1:(i-1)]),1,pi[i]/sum(pi[i:r])))
  }
  x = rbind(x,tx)
}
apply(x,2,mean)


###Ex2.2.3
set.seed(1)
n=50; r = 20
pi = c(rep(0.02,10),rep(0.08,10))
Fx=cumsum(pi); y=NULL
for (i in 1:n){
  r0=runif(1,0,1)
  if (r0<=pi[1]){
    y[i]=1
  }
  else y[i]=min(which(Fx>=r0))
}
x = c()
for(i in 1:r){
  x = c(x,sum(y==i))
}