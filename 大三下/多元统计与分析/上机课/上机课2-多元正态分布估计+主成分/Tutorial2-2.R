# Chapter 9-主成分分析

####################
# Exam 1: 相关系数矩阵
######################
library(mvtnorm)
library(MASS) 
library(corrplot)
library(lattice)
n<-1000
p<-10
mu<-rep(1,p)
rho<-0.5
corrmat <- diag(rep(1-rho, p)) + matrix(rho, p, p)
X       <- rmvnorm(n,mu,corrmat)

corrplot(cor(X), type = "lower", diag=FALSE, tl.col = "black", tl.srt = 45)

rgb.palette = colorRampPalette(c("white", "black"), space = "rgb")
levelplot(cor(X), las=1,main="Correlation matrix", xlab="", ylab="",
          col.regions=rgb.palette(120), cuts=100, at=seq(0,1,0.01))

#AR(1)相关系数矩阵
n<-1000
p<-10
mu<-rep(1,p)
rho<-0.5
ar1mat <- rho^outer(1:p, 1:p, function(x,y) abs(x-y))
X      <- rmvnorm(n, mu, ar1mat)
corrplot(cor(X), type = "lower", diag=FALSE, tl.col = "black", tl.srt = 45)

rgb.palette <- colorRampPalette(c("white", "black"), space = "rgb")
levelplot(cor(X), las=1,main="Correlation matrix", xlab="", ylab="",
          col.regions=rgb.palette(120), cuts=100, at=seq(0,1,0.01))



####################
# Exam 2
######################
Sig = matrix(c(1,-2,0,-2,6,0,0,0,2),ncol=3,nrow=3)
ev  = eigen(Sig)
val = round(ev$val,2)
vec = round(ev$vec,3)

#贡献率和累计贡献率
round(val/sum(val),4)           ###贡献率 
round(cumsum(val)/sum(val),4)   ###累计贡献率

rho11 = (vec[1,1]*sqrt(val[1]))/sqrt(Sig[1,1])
rho12 = (vec[2,1]*sqrt(val[1]))/sqrt(Sig[2,2])
rho13 = (vec[3,1]*sqrt(val[1]))/sqrt(Sig[3,3])

rho1 = c(round(rho11,3),round(rho12,3),round(rho13,3))
v1 = round(rho1^2,3)

rho21 = (vec[1,2]*sqrt(val[2]))/sqrt(Sig[1,1])
rho22 = (vec[2,2]*sqrt(val[2]))/sqrt(Sig[2,2])
rho23 = (vec[3,2]*sqrt(val[2]))/sqrt(Sig[3,3])

rho2 = c(round(rho21,3),round(rho22,3),round(rho23,3))
v2   = round(rho2^2,3)


####################
# Exam 3: 基于协方差矩阵Sigma的主成分分析(PCA)
######################
Sig = matrix(c(1,4,4,100),ncol=2,nrow=2)
ev.Sig  = eigen(Sig)
val.Sig = round(ev.Sig$val,2)
vec.Sig = round(ev.Sig$vec,3)

f1.Sig = round(val.Sig/sum(val.Sig),4)          ### 贡献率
fm.Sig = round(cumsum(val.Sig)/sum(val.Sig),4)  ### 累计贡献率

rho11.Sig = (vec.Sig[1,1]*sqrt(val.Sig[1]))/sqrt(Sig[1,1])
rho12.Sig = (vec.Sig[2,1]*sqrt(val.Sig[1]))/sqrt(Sig[2,2])

rho1.Sig = c(round(rho11.Sig,3),round(rho12.Sig,3))
v1.Sig   = round(rho1.Sig^2,3)


####################
# Exam 4: 基于相关系数矩阵R的主成分分析(PCA)
######################
Corr = matrix(c(1,0.4,0.4,1), ncol=2, nrow=2)
ev.Corr  = eigen(Corr)
val.Corr = round(ev.Corr$val,2)
vec.Corr = round(ev.Corr$vec,3)

f1.Corr = round(val.Corr[1]/sum(val.Corr),4)

rho11.Corr = (vec.Corr[1,1]*sqrt(val.Corr[1]))/sqrt(Corr[1,1])
rho12.Corr = (vec.Corr[2,1]*sqrt(val.Corr[1]))/sqrt(Corr[2,2])

rho1.Corr = c(round(rho11.Corr,3),round(rho12.Corr,3))
v1.Corr = round(rho1.Corr^2,3)


####################
# Exam 5: 学生(student) 数据分析
######################
student = data.frame(
  X1=c(148,139,160,149,159,142,153,150,151,139,140,161,158,140,137,
       152,149,145,160,156,151,147,157,147,157,151,144,141,139,148),
  X2=c(41, 34, 49, 36, 45, 31, 43, 43, 42, 31, 29, 47, 49, 33, 31, 
       35, 47, 35, 47, 44, 42, 38, 39, 30, 48, 36, 36, 30, 32, 38),
  X3=c(72, 71, 77, 67, 80, 66, 76, 77, 77, 68, 64, 78, 78, 67, 66, 
       73, 82, 70, 74, 78, 73, 73, 68, 65, 80, 74, 68, 67, 68, 70),
  X4=c(78, 76, 86, 79, 86, 76, 83, 79, 80, 74, 74, 84, 83, 77, 73, 
       79, 79, 77, 87, 85, 82, 78, 80, 75, 88, 80, 76, 76, 73, 78)
)
student.pca = princomp(student, cor=TRUE)
summary(student.pca, loadings=TRUE)

predict(student.pca)

par(mfrow=c(1,2))
screeplot(student.pca, type="lines", main="Scree Plot")
biplot(student.pca)


####################
# Exam 6: 身体数据例子
######################
x = c(1.00, 
      0.79, 1.00, 
      0.36, 0.31, 1.00, 
      0.96, 0.74, 0.38, 1.00, 
      0.89, 0.58, 0.31, 0.90, 1.00, 
      0.79, 0.58, 0.30, 0.78, 0.79, 1.00, 
      0.76, 0.55, 0.35, 0.75, 0.74, 0.73, 1.00, 
      0.26, 0.19, 0.58, 0.25, 0.25, 0.18, 0.24, 1.00,
      0.21, 0.07, 0.28, 0.20, 0.18, 0.18, 0.29,-0.04, 1.00,
      0.26, 0.16, 0.33, 0.22, 0.23, 0.23, 0.25, 0.49,-0.34, 1.00, 
      0.07, 0.21, 0.38, 0.08,-0.02, 0.00, 0.10, 0.44,-0.16, 0.23, 1.00, 
      0.52, 0.41, 0.35, 0.53, 0.48, 0.38, 0.44, 0.30,-0.05, 0.50, 0.24, 1.00, 
      0.77, 0.47, 0.41, 0.79, 0.79, 0.69, 0.67, 0.32, 0.23, 0.31, 0.10, 0.62, 1.00, 
      0.25, 0.17, 0.64, 0.27, 0.27, 0.14, 0.16, 0.51, 0.21, 0.15, 0.31, 0.17, 0.26, 1.00, 
      0.51, 0.35, 0.58, 0.57, 0.51, 0.26, 0.38, 0.51, 0.15, 0.29, 0.28, 0.41, 0.50, 0.63, 1.00, 
      0.21, 0.16, 0.51, 0.26, 0.23, 0.00, 0.12, 0.38, 0.18, 0.14, 0.31, 0.18, 0.24, 0.50, 0.65, 1.00)

names = c("X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8", "X9",  
          "X10", "X11", "X12", "X13", "X14", "X15", "X16")
#将矩阵生成相关矩阵
R = matrix(0, nrow=16, ncol=16, dimnames=list(names, names))
for (i in 1:16){
  for (j in 1:i){
    R[i,j]=x[(i-1)*i/2+j]; R[j,i]=R[i,j]
  }
}

pr   = princomp(covmat=R) 
load = loadings(pr)
pr

load
plot(load[,1:2]); text(load[,1], load[,2], adj=c(-0.4, 0.3))


####################
# Exam 7: 对R语言中自带的数据state进行分析
######################

help(state)
state.x77

#生成数据框数据
state.x77.df = data.frame(state.x77)

names(state.x77.df) = c("Popul", "Income", "Illit", "LifeExp", "Murder", "HSGrad", "Frost", "Area")
attach(state.x77.df)

state.pc = princomp(state.x77.df, cor=T)
#显示主成分的系数
summary(state.pc, loadings=T)

# 显示相关系数矩阵特征值
(state.pc$sdev)^2
#绘制碎石图
plot(1:(length(state.pc$sdev)),  (state.pc$sdev)^2, type='b', 
     main="碎石图", xlab="主成分个数", ylab="特征值")

par(pty="s")
plot(state.pc$scores[,1], state.pc$scores[,2], ylim=range(state.pc$scores[,1]), 
     xlab="PC 1", ylab="PC 2", type ='n', lwd=2)

text(state.pc$scores[,1], state.pc$scores[,2], labels=state.abb, cex=0.7, lwd=2)

par(pty="s")
plot(state.pc$scores[,1], state.pc$scores[,2], 
     xlab="PC 1", ylab="PC 2", type ='n', lwd=2)

text(state.pc$scores[,1], state.pc$scores[,2], labels=state.abb, cex=0.7, lwd=2)

#前两个主成分得分的biplot
biplot(state.pc,xlabs=state.abb)


####################
# Exam 8
######################
#### Inference about Principal Components:
#### CIs for eigenvalue(s) of Sigma:
#### (Note these eigenvalues will be different from the eigenvalues in the PCA done above, which was based on the *correlation* matrix R, not on S.)
lambda.CI=function(X, label.vec, conf.level=0.95){
  alpha    <- 1-conf.level
  n        <- nrow(X)
  m        <- length(label.vec)
  z        <- qnorm(alpha/(2*m), lower=F)
  lambdas  <- princomp(X)$sdev^2
  print(lambdas)
  LCL.M1   <- lambdas[label.vec]/(1+z*sqrt(2/n))
  UCL.M1   <- lambdas[label.vec]/(1-z*sqrt(2/n))
  
  LCL.M2   <- lambdas[label.vec]/exp(z*sqrt(2/n))
  UCL.M2   <- lambdas[label.vec]*exp(z*sqrt(2/n))
  
  CI.M1    <- cbind(LCL.M1, UCL.M1)
  CI.M2    <- cbind(LCL.M2, UCL.M2)
  return(list(Method1=CI.M1, Method2=CI.M2))
}

# Use the student data
student = data.frame(
  X1=c(148,139,160,149,159,142,153,150,151,139,140,161,158,140,137,
       152,149,145,160,156,151,147,157,147,157,151,144,141,139,148),
  X2=c(41, 34, 49, 36, 45, 31, 43, 43, 42, 31, 29, 47, 49, 33, 31, 
       35, 47, 35, 47, 44, 42, 38, 39, 30, 48, 36, 36, 30, 32, 38),
  X3=c(72, 71, 77, 67, 80, 66, 76, 77, 77, 68, 64, 78, 78, 67, 66, 
       73, 82, 70, 74, 78, 73, 73, 68, 65, 80, 74, 68, 67, 68, 70),
  X4=c(78, 76, 86, 79, 86, 76, 83, 79, 80, 74, 74, 84, 83, 77, 73, 
       79, 79, 77, 87, 85, 82, 78, 80, 75, 88, 80, 76, 76, 73, 78)
)

#95% CI for variance of the first population PC
lambda.CI(student,1)

#Joint 95% CIs for variances of the first two population PCs
lambda.CI(student,c(1,2))

#Joint 95% CIs for variances of the first four population PCs
lambda.CI(student,c(1,2,3,4))

#Note the first interval is wider because the FAMILYWISE confidence coefficient is 95% here.
# 99% CI for variance of the first population PC
lambda.CI(student,1, conf.level=0.99)



####################
# Exam 9: 不相关变量的例子
######################
library(mvtnorm)
library(MASS)
pca     <- function(n, p, rho, mu){
  corrmat = diag(rep(1-rho, p)) + matrix(rho, p, p)
  X = rmvnorm(n,mu,corrmat)
  pca = princomp(X, cor=TRUE)
  return(pca)
}

# A scree plot
n<-10000
p<-5
mu<-rep(1,p)
pca.r1 <- pca(n, p, rho=0, mu)
pca.r2 <- pca(n, p, rho=0.5, mu)

par(mfrow=c(1,2))
screeplot(pca.r1, type="lines", ylim=c(0,3), main="Meaningless Scree Plot")
legend("topright",legend=c("rho=0"))
screeplot(pca.r2, type="lines", ylim=c(0,3), main="Scree Plot")
legend("topright",legend=c("rho=0.5"))


plot(1:(length(pca.r1$sdev)),  (pca.r1$sdev)^2, type='b', ylim=c(0,3),
     main="Meaningless Scree Plot", xlab="Number of Components", ylab="Eigenvalue")
legend("topright",legend=c("rho=0"))
plot(1:(length(pca.r2$sdev)),  (pca.r2$sdev)^2, type='b', 
     main="Scree Plot", xlab="Number of Components", ylab="Eigenvalue")
legend("topright",legend=c("rho=0.5"))



####################
# Exam 10: PCA application
# Data Visualization
# Fisher iris data
######################
X <- iris[,1:4]
groupid <- iris[,5]
X.pca   <- princomp(~.,data=X)
summary(X.pca,loadings=TRUE)

plot(X.pca$scores[,1], X.pca$scores[,2], xlab="PC1", ylab="PC2", pch=rep(1:3,each=50), col=groupid, main="Iris data")
legend(2,1.1,legend=levels(groupid), pch=1:3, col=1:3)



####################
# Exam 11: 图像处理  这个例子缺图片
######################
library(jpeg)
Disney<-readJPEG('disney.jpg') # 图片数据不存在，换个图片
dim(Disney)

Disney.pca=list()
for(i in 1:3)
  Disney.pca[[i]]=prcomp(Disney[,,i], center = FALSE)
imge=list()
for (i in c(3,10,100)) {
  imge[[i]] = sapply(Disney.pca, function(y) {
    comimge = y$x[,1:i] %*% t(y$rotation[,1:i])
  }, simplify = 'array')
  writeJPEG(imge[[i]], paste('compDisney', round(i,0), '.jpg', sep = ''))
}

FS = file.info('Disney.jpg')$size
FS 

z = NULL
for(i in c(3,10,100))
  z = c(z,file.info(paste("compDisney",i,".jpg",sep=""))$size/FS)
z


####################
# Exam 12: 图像处理
######################
library(jpeg)
PIC=readJPEG('flower.jpg')
PICsvd = list()
for(i in 1:3) PICsvd[[i]] = svd(PIC[,,i])
comPIC = PIC; cPIC = list()
PS = file.info('flower.jpg')$size
PS 

for (i in c(5, 20, 100)){
  for(k in 1:3){  
    comPIC[,,k]=PICsvd[[k]]$u[,1:i] %*% diag(PICsvd[[k]]$d[1:i]) %*%
      t(PICsvd[[k]]$v[,1:i])
    writeJPEG(comPIC, paste('comPIC', round(i,0), '.jpg', sep = ''))
  }
  cPIC[[i]]=comPIC
}
z = NULL
for(i in c(5, 20, 100))
  z = c(z,file.info(paste("comPIC",i,".jpg",sep=""))$size/PS)
z



####################
# Exam 13: 人脸识别
# PCA image recognition
######################

library(RColorBrewer)
library(rtiff) # 安装不成功 'rtiff‘包不适用于新版本的R 4.0.3 
# install triff package locally

showMatrix = function(x) 
  image(t(x[nrow(x):1,]), xaxt = 'none', yaxt = 'none', 
        col=rev(colorRampPalette(brewer.pal(7,'Greys'))(100)))

# set path
path = "E:/face"
fileNames = dir(path) 
filePath = sapply(fileNames, function(x){ 
  paste(path,x,sep='/')})   
data = lapply(filePath, function(x){
  as.vector(readTiff(x)@red)})
data = data.frame(data)
X = t(data)
#展示40个原始人脸图像
par(mfrow=c(8,5),mar = c(0, 0, 0, 0))
for(i in 1:40){
  face.mat = matrix(X[i,],538,438)
  showMatrix(face.mat)
}

n= nrow(X); p=ncol(X)
X.C = scale(X,scale=FALSE)    ## 中心化
Y = t(X.C)/sqrt(n-1)          ## 作变换得到Y 
Y.egn = eigen(t(Y)%*%Y)        
Phi = Y%*%Y.egn$vectors
Phi = apply(Phi,2,function(i) i/sqrt(sum(i*i)))  
m = 25                        ## 选25个主成分，累计贡献率达到89.2%
sum(Y.egn$value[1:m])/sum(Y.egn$value)

Phi.m = Phi[,1:m]; Y.hat = X.C%*%Phi.m
mu = apply(X, 2, mean)
#恢复图像
X.R = Y.hat%*%t(Phi.m)+matrix(mu,nrow=n,ncol=p,byrow=TRUE)
#展示特征脸(Eigenface)
par(mfrow = c(5,5), mar = c(0, 0, 1, 0), bty = 'n')
for(i in 1:25){ showMatrix(matrix(Phi.m[,i],538,438)) }

# 重构所有的40个人脸图像
par(mfrow = c(8,5), mar = c(0, 0, 0, 0), bty = 'n')
for(i in 1:40){ showMatrix(matrix(X.R[i,],538,438)) }

#比较第3个原始人脸图像和恢复后的图像
face.3 = matrix(X[3,],538,438)
face.3.R = matrix(X.R[3,],538,438)
par(mfrow = c(1,2), mar = c(0, 0, 0, 0), bty = 'n')
showMatrix(face.3);  showMatrix(face.3.R)
