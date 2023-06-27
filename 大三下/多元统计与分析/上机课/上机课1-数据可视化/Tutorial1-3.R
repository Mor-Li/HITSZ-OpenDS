##  Chapter 3-多元正态分布
## 
##
#################


####################
# Exam 1: 二元正态分布的密度函数和等高线
# 
######################

library(mvtnorm)
library(latex2exp)
rho <- -0.75
mu <- c(0,0)
sig1 = sig2 = 1
Sigma<-matrix(c(sig1^2,rho*sig1*sig2,rho*sig1*sig2,sig2^2),nrow = 2)
X <- seq(-3,3,length=41);  Y = X  
f <- function(X, Y) {
  XY <- cbind(X, Y)
  dmvnorm(XY, mu, Sigma)
}
Z <- outer(X, Y, f)
persp(X, Y, Z, col="lightgreen", theta=30, phi=20, r=50, d=0.1,
      expand=0.5, ltheta=90, lphi=180, shade=0.75, ticktype="detailed",
      nticks=5, xlab = "X", ylab = "Y", zlab = "f(x,y)")
legend("topright",legend = TeX("$\\rho = -0.75$"))


win.graph()
image(X,Y,Z)                ## 色阵图
contour(X,Y,Z,add=TRUE)     ## 等高线图
legend("topright",legend = TeX("$\\rho = -0.75$"))

win.graph()
contour(X,Y,Z)

win.graph()
image(X,Y,Z)


####################
# Exam 2: 3D
# 
######################
library(rgl)
library(webshot)
persp3d(X, Y, Z, theta=45, phi=25, col='red')
M <- par3d("userMatrix")
bg3d("white")
play3d(par3dinterp(userMatrix=list(M,rotate3d(M, angle=pi,x=1,y=0,z=0))), duration=10) 



####################
# Exam 3: Density function for different rho
# 
######################
f = function (X, Y = X, mu1=0, mu2=0, sig1=1, sig2=2, rho = 0) 
{
  XoY = ((X-mu1)^2/sig1^2 - 2 * rho * (X-mu1) * (Y-mu2)/(sig1*sig2) + (Y-mu2)^2/sig2^2)/(2 * (1 - rho^2))
  density = exp(-XoY)/(2 * pi * sig1*sig2*sqrt(1 - rho^2))
  density
}
for(i in seq(-0.9, 0.9, by=.1) )
{
  Z=outer(X, Y, function(X,Y)f(X,Y,mu1=0,mu2=0,sig1=1,sig2=1,rho=i))
  persp(X, Y, Z, theta=0, phi=25, col='salmon2', main=paste("rho=",as.character(i)))
  locator(1)
}


####################
# Exam 4: Density function for different sigma1
# 
######################
for(sigma1 in seq(0.1, 5, by=.5) )
{
  Z=outer(X,Y,function(X,Y)f(X,Y,mu1=0,mu2=0,sig1=sigma1,sig2=1,rho=0))
  persp(X, Y, Z, theta=45, phi=25, col='salmon2', main=paste("sigma1=",as.character(sigma1)))
  locator(1)
}




####################
# Exam 5: Contour Plot
# 
######################

par(mfrow=c(2,2))
X = Y = seq(-4, 4, length=100)
Z = outer(X, Y, function(X, Y)f(X, Y))
contour(Z, main="independent")
Z = outer(X, Y, function(X, Y)f(X, Y, rho=0.5))
contour(Z,main="rho=0.5")
Z = outer(X, Y, function(X, Y)f(X, Y, sig1=4, rho=0.5))
contour(Z, main="sig1=4 and rho=0.5")
Z = outer(X, Y, function(X, Y)f(X, Y, sig1=4, rho=-0.9))
contour(Z, main="sig1=4 and rho=-0.9")


####################
# Exam 6: Simulate a bivariate normal distribution
# 
######################

library(MASS) 
library(mvtnorm)
mu    <- c(0,2)
Sig   <- matrix(c(1,0.5,0.5,1), 2, 2)
n     <- 1000
biv1  <- mvrnorm(n, mu, Sig)
colnames(biv1) <- c("X","Y")
#### Using X=mu+Sig^{1/2}Y to obtain the data
#### mu is the mean vector
#### Y come from normal distribution IID
#### Sig = LL, where L=U*Lambda*t(U)
sig.eigen <- eigen(Sig)
Sigma     <- sig.eigen$vectors%*%diag(sqrt(sig.eigen$values))%*%t(sig.eigen$vectors)
biv2      <- mu+t(matrix(rnorm(2*n),ncol=2)%*%Sigma)
biv2      <- t(biv2)

par(mfrow=c(2,2))
hist(biv1[,1],main="Histogram of X")
hist(biv1[,2],main="Histogram of Y")

hist(biv2[,1],main="Histogram of X")
hist(biv2[,2],main="Histogram of Y")


#install.packages("mixtools")
library(mixtools)
# 安装有问题。
par(mfrow=c(1,2))
plot(biv1)
ellipse(mu = colMeans(biv1),sigma = cov(biv1),alpha=.05,col='red')
points(t(mu),col='red',pch=19)


plot(biv2)
ellipse(mu = colMeans(biv2),sigma = cov(biv2),alpha=.05,col='red')
points(t(mu),col='red',pch=19)


library(mvtnorm)
mu    = c(0,2)
Sig   = matrix(c(1,0.5,0.5,1), 2, 2)
n     = 1000
biv1  = rmvnorm(n, mu, Sig)
colnames(biv1) = c("X","Y")
#### using X=mu+Sig^{1/2}Y to obtain the data
#### mu is the mean vector
#### Y come from normal distribution IID
#### Sig = LL, where L=U*Lambda*t(U)
sig.eigen = eigen(Sig)
Sig1      = sig.eigen$vectors%*%diag(sqrt(sig.eigen$values))%*%t(sig.eigen$vectors)
biv2      = mu+t(matrix(rnorm(2*n),ncol=2)%*%Sig)
biv2      = t(biv2)

par(mfrow=c(2,2))
hist(biv1[,1],main="Histogram of X")
hist(biv1[,2],main="Histogram of Y")

hist(biv2[,1],main="Histogram of X")
hist(biv2[,2],main="Histogram of Y")


#install.packages("mixtools")
library(mixtools)
# 安装有问题

plot(biv1)
ellipse(mu = colMeans(biv1),sigma = cov(biv1),alpha = .05,col='red')
points(t(mu),col = 'red',pch = 19)

plot(biv2)
ellipse(mu = colMeans(biv2),sigma = cov(biv2),alpha = .05,col = 'red')
points(t(mu),col = 'red',pch = 19)


####################
# Exam 7: Bivariate Normal Distribution
# 
######################
#install.packages("fMultivar")
library(fMultivar)
rnorm2d(n, rho=0)
pnorm2d(x=0, y=0, rho=0)
dnorm2d(x=0, y=0, rho=0)

m <- 3
Sig <- diag(3)
Sig[2,1] <- 3/5
Sig[3,1] <- 1/3
Sig[3,2] <- 11/15
pmvnorm(lower=rep(-Inf, m), upper=c(1,4,2), mean=rep(0, m), corr=Sig)

qmvnorm(0.95, sigma = diag(2), tail = "both")

X <- rmvnorm(500, c(1,2,1), Sig)
colMeans(X)

var(X)



####################
# Exam 8: Show the density function curve changes
# Variable Description:
#  mu —— mean vector;
# Sig —— covariance matrix;
# x, y, z —— z=f(x,y), where f is the density function.
######################

### require packages: mvtnorm and animation
library(animation)
library(mvtnorm)

# Case 1: The density function for the bivariate normal distribution
mu  <- c(0,0)    
Sig <- diag(2)   

x = y = seq(-4, 4, length=40)
z <- matrix(apply(expand.grid(x, y), 1, function(v){dmvnorm(v,mu,Sig)}), nrow = 40, byrow = T)

# Plot the 2D density function
persp(x,y,z,theta=45,phi=25,col='yellow',border = "black")

# Case 2: Show density function curve changes when mu[1] varies
mu  = c(-3,0)
Sis = diag(2)
# Draw 30 pictures to generate GIF using “saveHTML”

# saveHTML({ 
  for(i in 1:30){
    mu[1] = mu[1] + 0.1
    x = y = seq(-4, 4, length= 40)
    z = matrix(apply(expand.grid(x, y), 1, function(v){dmvnorm(v,mu,Sig)}), nrow = 40, byrow = T)
    persp(x, y, z, theta=45, phi=25, col='yellow', zlim = range(0,0.3), main = paste("mu[1] =", round(mu[1],3)))
  }
})

# Case 4: Show the density function curve changes as Sig[1,2] and Sig[2,1] vary simultaneously

mu  = c(0,0)
Sig = 1.9*diag(2) - 0.9 

#saveHTML(
#  for(i in 1:30){
    Sig[1,2] = Sig[2,1] = Sig[1,2] + 0.06 
    x = y = seq(-4, 4, length=30)
    z = matrix(apply(expand.grid(x,y), 1, function(v){dmvnorm(v,mu,Sig)}), nrow = 30)
    persp(x, y, z, theta=45, phi=25, col='yellow', border = "black", zlim = range(0,0.3), main = paste("  correlation = ", round(Sig[1,2],3)))
  }
#)

