# Chapter 5-多元正态分布的参数估计

####################
# Exam 1: MLEs for Multivariate Normal Distribution
######################
mu  <- c(0, 1, 2)
Sig <- matrix(c(1, 0.6, 0, 0.6, 1, -0.4, 0, -0.4, 1), 3, 3)
n   <- 1000
library(MASS) 
library(mvtnorm)
X  <- mvrnorm(n, mu, Sig)
S  <- cov(X)               ### sample covariance matrix
mu_mle  <- colMeans(X)     ### mle for mean
Sig_mle <- (n-1)*S/n       ### mle for covariance matrix = V/n
mu_mle

Sig_mle

vars <- diag(sqrt(1/diag(Sig_mle)))
Rhat <- vars%*%Sig_mle%*%vars  ### mle for correlation matrix
Rhat

cor(X)      ### for comparison purpose between Rhat and cor(X)

Rhat-cor(X)


####################
# Exam 2: Hypothesis testing H0: rho13 = 0; H1: rho13 ≠ 0
######################

rho.hat  =  Rhat[1,3]
#### test statisitc
t.stat = rho.hat*sqrt((n-2)/(1-rho.hat^2))
#### compute p vlaue
2-2*pt(abs(t.stat), df=n-2) 



####################
# Exam 3: Confidence interval of rho13
######################
zl <- rho.hat-qnorm(0.975)/sqrt(n-3)
zu <- rho.hat+qnorm(0.975)/sqrt(n-3)
lower <- (exp(2*zl)-1)/(exp(2*zl)+1)
upper <- (exp(2*zu)-1)/(exp(2*zu)+1)
print(c(lower,upper))


####################
# Exam 4: Wishart distribution
######################

S   =  toeplitz((10:1)/10)
set.seed(11)
W   = rWishart(1000, df=20, Sigma=S)
dim(W)                                 ####  10 10  1000
mW  = apply(W, 1:2, mean)              #### ~= E[Wish(W, 20)] = 20 * W


####################
# Exam 5: Wishart distribution: A test for covariance matrix
######################

Sig = matrix(c(1, 0.8, 0.4, 0.8, 1, 0.2, 0.4, 0.2, 1), nrow = 3)
p   = dim(Sig)[1]
df  = 4
Sig.vec = c(Sig)
#Empirical (Using the simulated data)
set.seed(9524)
reps  =  100000                       #### number of obs in our sampling dist
W.empr = rWishart(reps, df=df, Sigma=Sig)
W.empr.vec = aperm(W.empr, c(3,2,1))  #### convert tha array to matrix
dim(W.empr.vec) = c(reps, p*p)

round(apply(W.empr.vec,2, mean), 2)   #### sample mean

df*Sig.vec                            #### theoretical mean

#Comparet the variance
Eij = function(n, i, j){
  Eij = matrix(0, n, n)
  Eij[i,j] = 1
  Eij
}
K = matrix(0, p*p, p*p)
for(i in 1:p)
  for(j in 1:p) {
    EIJ = Eij(p, i, j)
    K = K+kronecker(EIJ, t(EIJ))
  }

W.theor = df*(diag(p*p)+K)%*%kronecker(Sig,Sig)

round(cov(W.empr.vec), 2)      #### sample covriance matrix of Wishart samples
W.theor 




####################
# Exam 6: Test of Normality
######################

#Exam 6.1: Test of Normality for univariate
#QQplot

par(mfrow=c(2,2));set.seed(16);
X = rnorm(50);qqnorm(X);qqline(X);legend("topleft",legend=c("n=50"))
X = rnorm(100);qqnorm(X);qqline(X);legend("topleft",legend=c("n=100"))
X = rnorm(1000);qqnorm(X);qqline(X);legend("topleft",legend=c("n=1000"))
X = rnorm(10000);qqnorm(X);qqline(X);legend("topleft",legend=c("n=10000"))

#Exam 6.2: See the data set in Table 4-1 of Johnson and Wichern (2008)
# change directory to code file
mdat <- read.table("T4-1.dat",header=F)
mdat <- cbind(1:nrow(mdat), mdat)
names(mdat) <- c('Oven', 'Radiation')
head(mdat)

mmean = mean(mdat$Radiation)
mmean

mvar  = var(mdat$Radiation)
mvar


qqnorm(mdat$Radiation, pch = 20, main="Normal Probability Plot") 
qqline(mdat$Radiation)

library(qqtest)
par(mfrow=c(1,2))
library(qqtest)
qqtest(mdat$Radiation,main="Radiation")


# install.packages("nortest")
library(nortest)
library(help='nortest')
shapiro.test(mdat$Radiation)


mdat$logRad = log(mdat$Radiation)   
hist(mdat$logRad)

qqnorm(mdat$logRad, pch = 19, main="Normal Probability Plot") 
qqline(mdat$logRad)

qqtest(mdat$logRad,main="Log Radiation")
shapiro.test(mdat$logRad)
lillie.test(mdat$logRad)

# Exam 6.3: Test normality for multivariates
# Chi squred distribution Q-Q plot

library(MASS)
mu    <- c(0,1)
Sig   <- matrix(c(1, 0.6, 0.6, 1), 2, 2)
n     <- 1000

biv   <- mvrnorm(n, mu, Sig)
Sbiv  <- cov(biv)
D2    <- mahalanobis(biv, colMeans(biv), Sbiv)

qqplot(qchisq(ppoints(n), df = 2), D2,
       main = expression("Q-Q plot for" ~~ {chi^2}[nu == 2]))
abline(c(0,1))

chiqqplot = function(x){
  if(!is.matrix(x))
    x  = as.matrix(x)
  n  = nrow(x); p = ncol(x)
  D2 = mahalanobis(x,colMeans(x),cov(x))
  qqplot(qchisq(ppoints(n), df = p), D2,xlab="Theoretical Q of Chisquare", ylab="Mah distance",
         main = expression("Q-Q plot for" ~~ {chi^2}[p]),pch=19)
  abline(c(0,1))
}

chiqqplot(mdat$logRad)

# Exam 6.4: Multiple hypothesis testing
testnormality = function(X, numproj = 1000)
{
  p = ncol(X); n = nrow(X)
  x = matrix(rnorm(numproj * p), nrow = p)
  #### Generate 1,000 standard p-variate normal random variables.
  y = matrix(sqrt(apply(x^2, 2, sum)), nrow = p, ncol = numproj, by = T)
  z = x / y
  tempdat = as.matrix(X) %*% z
  #### This gives rise to a p x numproj matrix
  #### Perform Shapiro-Wilks' test and calculate individual p-values on each of the numproj observation sets.
  pvals = as.numeric(matrix(unlist(apply(tempdat,2,shapiro.test)),ncol=4,
                            by =T)[,2])
  #### Multiple hypthesis testing
  return(min(p.adjust(pvals,method="BH")))
}

pvals <- testnormality(mdat, numproj = 10000)
pvals

pvals = testnormality(biv, numproj =10000)
pvals


# Exam 6.5: Energy Statistics

library(energy)
mvnorm.etest(mdat)
data(iris)
mvnorm.e(iris[1:50, 1:4])
normal.e(iris[1:50, 1])

## Test if the iris Setosa data has multivariate normal distribution
mvnorm.etest(iris[1:50,1:4], R = 199)
## Test a univariate sample for normality
x = runif(50, 0, 10)
mvnorm.etest(x, R = 199)


# Example 6.6: Test of Normality
# Data set in Table 5-1 of Johnson and Wichern (2008)
# change directory to file
x <- read.table("T5-1.dat")
qqnorm(x[,1])
qqnorm(x[,2])
qqnorm(x[,3])
pvals = testnormality(mdat)
pvals
mvnorm.etest(x, R = 199)

# 多元正态分布的离群点检测
# install.packages("mvoutlier")
library(mvoutlier)
library(help='mvoutlier')
load(file = "3dExample.rda")
pairs(dat)

#引入离群点
outFactor = 1.5
dat1 = rbind(dat, outFactor*c(-1,-1.2,0.7))
pairs(dat)
pairs(dat1, col = c(rep(1,300), 2), pch = c(rep(1,300), 3), cex = c(rep(1,300), 2))

library(mvoutlier)
chisq.plot(dat)
abline(c(0,1))

library(rgl)
plot3d(dat1, col = c(rep(1,300), 2))
# 自动检测离群点
aq.plot(dat)


####################
# Exam 7:  box-cox变换
######################

m1  = read.table(file ="T4-1.dat", header=F)    #### microwave.door.close
m2  = read.table("T4-5.dat", header=F)          #### microwave.door.open

# install.packages("car")
library(car)
mdat1 = as.matrix(cbind(m1,m2))
colnames(mdat1) = c("close", "open") 
bc  = powerTransform(mdat1~1)                   #### Find the optimal box-cox parameter vector lambda 
summary(bc)

bc.mdat = bcPower(mdat1,bc$lambda)              #### Save the transformed values
#检验正态性
library(energy)
plot(bc.mdat)
#chiqqplot(bc.mdat) 这是matlab命令
mvnorm.etest(bc.mdat,R=999)

#非参数密度函数估计
library(MASS)
f1 <- kde2d(bc.mdat[,1],bc.mdat[,2], h = c(width.SJ(bc.mdat[,1]), width.SJ(bc.mdat[,2])))
persp(f1, phi = 30, theta = 20, d = 5)

#对结果进行比较
bc.close  = powerTransform(mdat1[,1])
bc.nclose = bcPower(mdat1[,1],bc.close$lambda)
bc.open   = powerTransform(mdat1[,2])
bc.nopen  = bcPower(mdat1[,2],bc.open$lambda)
bc.ndat   = cbind(bc.nclose,bc.nopen)
#对不同lambda的结果进行比较
bc$lambda

c(bc.close$lambda, bc.open$lambda)

#检验正态性
plot(bc.ndat)
chisq.plot(bc.ndat)
mvnorm.etest(bc.ndat,R=999)

#非参数密度函数估计
f2 = kde2d(bc.ndat[,1],bc.ndat[,2], h = c(width.SJ(bc.ndat[,1]), width.SJ(bc.ndat[,2])))
persp(f2, phi = 30, theta = 20, d = 5)
