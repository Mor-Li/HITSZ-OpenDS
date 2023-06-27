# Chapter 10-因子分析

# Data
names <- c('30年','20年','10年','5年','1年')
R <- matrix(c(1.0,0.98,0.92,0.85,0.66,0.98,1,0.92,0.86,0.67,0.92,0.92,1,0.90,0.71,0.85,0.86,0.90,1,0.84,0.66,0.67,0.71,0.84,1), 
           nrow=5, ncol=5, dimnames=list(names,names), byrow=T)


####################
# Exam 1: 主成分法(Principal Component Method)
######################

fa.pcm = function(S, k){
  p = nrow(S); diag_S = diag(S); sum_rank = sum(diag_S)
  rowname = rownames(S); colname = paste('Factor',1:k,sep='')
  A = matrix(0,nrow=p,ncol=k,dimnames=list(rowname,colname))
  eig = eigen(S)
  for (i in 1:k)
    A[,i] = sqrt(eig$values[i])*eig$vectors[,i]
  h = diag(A%*%t(A))
  rowname = c('SS loadings','Proportion Var','Cumulative Var')
  B = matrix(0,nrow=3,ncol=k,dimnames=list(rowname,colname))
  for (i in 1:k){
    B[1,i] = sum(A[,i]^2)
    B[2,i] = B[1,i]/sum_rank
    B[3,i] = sum(B[1,1:i])/sum_rank
  }
  A = round(A,3); B = round(B,3); h = round(h,3)
  diag_S = round(diag_S,3)
  method = c('Principal Component Method')
  list(method=method, loadings=A,
       var=cbind(common=h,spcific=diag_S-h), B=B)
}

fa.pcm.fit <- fa.pcm(R, k=2)
fa.pcm.fit

vm.pcm <- varimax(fa.pcm.fit$loadings, normalize=F)              ####（方差最大化旋转）
vm.pcm 

# 主成分法的因子得分
z <- c(1.4,-0.2,0.5,-1.0,0.8)
A <- fa.pcm.fit$loadings
D <- diag(fa.pcm.fit$var[,2])
I <- diag(c(1,1))
B.score <- solve(t(A)%*%solve(D)%*%A)%*%t(A)%*%solve(D)%*%z      #### Bartlett因子得分
T.score <- solve(I+t(A)%*%solve(D)%*%A)%*%t(A)%*%solve(D)%*%z    #### Thompsom因子得分
B.score


####################
# Exam 2: 主因子法(Iterative Principal Factor Method)(给初始值迭代法)
######################
fa.ipfm = function(R, k, d){
  p = nrow(R); diag_R = diag(R); sum_rank = sum(diag_R)
  rowname = rownames(R)          #paste('X',1:p,sep='')
  colname = paste('Factor',1:k,sep='')
  A = matrix(0,nrow=p,ncol=k,dimnames=list(rowname,colname))
  mmax = 200; m = 1; h = diag_R-d
  repeat{
    diag(R) = h; h1 = h; eig = eigen(R)
    for (i in 1:k)
      A[,i] = sqrt(eig$values[i])*eig$vectors[,i]
    h = diag(A%*%t(A))
    if ((sqrt(sum((h-h1)^2))<1e-4)|m==mmax) break
    m = m+1
  }
  rowname = c('SS loadings','Proportion Var','Cumulative Var')
  B = matrix(0,nrow=3,ncol=k,dimnames=list(rowname,colname))
  for (i in 1:k){
    B[1,i] = sum(A[,i]^2)
    B[2,i] = B[1,i]/sum_rank
    B[3,i] = sum(B[1,1:i])/sum_rank
  }
  A = round(A,3)
  B = round(B,3)
  h = round(h,3)
  diag_R = round(diag_R,3)
  method = c('Iterative Principal Factor Method')
  list(method=method,loadings=A,var=cbind(common=h,spcific=diag_R-h),B=B,iterative=m)
} 

d = c(0.12,0.15,0.09,0.05,0.07) 
fa.pfm.fit = fa.ipfm(R, k=2, d)
fa.pfm.fit

fa.ipfm = function(R, k, d){
  p = nrow(R); diag_R = diag(R); sum_rank = sum(diag_R)
  rowname = rownames(R)          #paste('X',1:p,sep='')
  colname = paste('Factor',1:k,sep='')
  A = matrix(0,nrow=p,ncol=k,dimnames=list(rowname,colname))
  mmax = 200; m = 1; h = diag_R-d
  repeat{
    diag(R) = h; h1 = h; eig = eigen(R)
    for (i in 1:k)
      A[,i] = sqrt(eig$values[i])*eig$vectors[,i]
    h = diag(A%*%t(A))
    if ((sqrt(sum((h-h1)^2))<1e-4)|m==mmax) break
    m = m+1
  }
  rowname = c('SS loadings','Proportion Var','Cumulative Var')
  B = matrix(0,nrow=3,ncol=k,dimnames=list(rowname,colname))
  for (i in 1:k){
    B[1,i] = sum(A[,i]^2)
    B[2,i] = B[1,i]/sum_rank
    B[3,i] = sum(B[1,1:i])/sum_rank
  }
  A = round(A,3)
  B = round(B,3)
  h = round(h,3)
  diag_R = round(diag_R,3)
  method = c('Iterative Principal Factor Method')
  list(method=method,loadings=A,var=cbind(common=h,spcific=diag_R-h),B=B,iterative=m)
} 

d <- c(0.12,0.15,0.09,0.05,0.07) 
fa.pfm.fit <- fa.ipfm(R, k=2, d)
fa.pfm.fit

# 主因子法的因子得分
z <- c(1.4,-0.2,0.5,-1.0,0.8)
A <- fa.pfm.fit$loadings
D <- diag(fa.pfm.fit$var[,2])
I <- diag(c(1,1))
B.score <- solve(t(A)%*%solve(D)%*%A)%*%t(A)%*%solve(D)%*%z
T.score <- solve(I+t(A)%*%solve(D)%*%A)%*%t(A)%*%solve(D)%*%z
B.score

T.score



####################
# Exam 3: 无迭代主因子法(Non-iterative Principal Factor Method)
# 初始值最大值相关系数
######################

fa.nipfm = function(R, k){
  p = nrow(R); diag_R = diag(R); sum_rank = sum(diag_R)
  rowname = rownames(R)          # paste('X',1:p,sep='')
  colname = paste('Factor',1:k,sep='')
  A = matrix(0,nrow=p,ncol=k,dimnames=list(rowname,colname))
  h = apply(abs(R-diag(1,nrow=p,ncol=p)),2,max)                 ##初始值 
  diag(R) = h; h1 = h;
  eig = eigen(R)
  for (i in 1:k){
    A[,i] = sqrt(eig$values[i])*eig$vectors[,i]
    h = diag(A%*%t(A))}
  
  rowname = c('SS loadings','Proportion Var','Cumulative Var')
  B = matrix(0,nrow=3,ncol=k,dimnames=list(rowname,colname))
  for (i in 1:k){
    B[1,i] = sum(A[,i]^2)
    B[2,i] = B[1,i]/sum_rank
    B[3,i] = sum(B[1,1:i])/sum_rank
  }
  A = round(A,3)
  B = round(B,3)
  h = round(h,3)
  diag_R = round(diag_R,3)
  method = c('Non-iterative Principal Factor Method')
  list(method=method,loadings=A,var=cbind(common=h,spcific=diag_R-h),B=B)
} 

fb.fit <- fa.nipfm(R, k=2)
fb.fit

vm.nipfm <- varimax(fb.fit$loadings, normalize=F)                ####(方差最大化旋转)
vm.nipfm


####################
# Exam 4: 极大似然法(Maximum likelihood method)
######################

fa.mlm = function(S, k, d){
  p = nrow(S); diag_S = diag(S); sum_rank = sum(diag_S)
  rowname = paste("X", 1:p, sep="")
  colname = paste("Factor", 1:k, sep="")
  A = matrix(0, nrow=p, ncol=k, 
             dimnames=list(rowname, colname))
  
  itmax = 20;  it = 1 
  repeat{
    d1 = d; d2 = 1/sqrt(d); eig = eigen(S * (d2 %o% d2))
    for (i in 1:k)
      A[,i] = sqrt(eig$values[i]-1)*eig$vectors[,i]
    A = diag(sqrt(d)) %*% A
    d = diag(S-A%*%t(A))
    if ((sqrt(sum((d-d1)^2))<1e-4)|it==itmax) break
    it = it + 1
  }
  
  rowname = c("SS loadings", "Proportion Var", "Cumulative Var")
  B = matrix(0, nrow=3, ncol=k, 
             dimnames=list(rowname, colname))
  for (i in 1:k){
    B[1,i] = sum(A[,i]^2)
    B[2,i] = B[1,i]/sum_rank
    B[3,i] = sum(B[1,1:i])/sum_rank
  }
  method = c("Maximum Likelihood Method")
  list(method=method, loadings=A, 
       var=cbind(common=diag_S-d, spcific=d), B=B, iterative=it) 
}   

d = c(0.12,0.15,0.09,0.05,0.07) 
fa.mlm.fit = fa.mlm(R, k=2, d)
fa.mlm.fit

vm.mlm = varimax(fa.mlm.fit$loadings, normalize=F)              ####(方差最大化旋转)
vm.mlm



####################
# Exam 5: Maximum likelihood method using function factanal()
######################
fa.mle.fit2 <- factanal(factors=2, covmat=R, rotation="none", n.obs=696)
fa.mle.fit2

vm.mlm2 = factanal(covmat=R, factors=2, rotation="varimax", n.obs=696)#（方差最大化旋转）
vm.mlm2

# 极大似然法因子得分
z = c(1.4,-0.2,0.5,-1.0,0.8)
A1 = fa.mle.fit2$loadings
A  = A1[1:5,]
D  = diag(diag(R-A%*%t(A)))
I  = diag(c(1,1))
B.score = solve(t(A)%*%solve(D)%*%A)%*%t(A)%*%solve(D)%*%z
T.score = solve(I+t(A)%*%solve(D)%*%A)%*%t(A)%*%solve(D)%*%z
B.score


####################
# Exam 6: The example in Lawley and Maxwell (1979)
######################

names = c('盖尔语','英语','历史','算数','代数','几何')
R.score = matrix(c(1.0, .439,  .410, .288, .329, .248,
                   .439, 1.0,  .351, .354, .320, .329,
                   .410, .351,  1.0, .164, .190, .181,
                   .288, .354, .164,  1.0, .595, .470,
                   .329, .320, .190, .595,  1.0, .464,
                   .248, .329, .181, .470, .464,  1.0), 
                 nrow=6, ncol=6, dimnames=list(names,names), byrow=T)

fa.fit = factanal(factors=2, covmat=R.score, rotation="none", n.obs=220)
fa.fit

#旋转20度后的载荷矩阵
A = fa.fit$loadings[1:6, ]
phi = 20/180*pi
G = matrix(c(cos(phi), -sin(phi), sin(phi), cos(phi)), nrow=2, ncol=2)
round(A%*%G, 3)

# 旋转20度直角坐标图10.1
library(ggplot2)  # 这个包装不上，需要高版本R
x = A[,1];  y = A[,2]
data = data.frame(x, y, ID=1:6)

ggplot(data, aes(x=x, y=y)) +  
  geom_point(size=2)+
  coord_cartesian(xlim=c(0, 1.2),ylim=c(-0.6,0.6))+
  geom_segment(mapping=aes(x=0, y=-0.6, xend=0, yend=0.55), arrow=arrow(length=unit(0.4,"cm")), size=.5, color="black",linetype=1)+
  geom_segment(mapping=aes(x=0, y=0, xend=1.1, yend=0), arrow=arrow(length=unit(0.4,"cm")), size=.5, color="black",linetype=1)+
  annotate('text',label='F1',x = 1.15,y=0)+
  annotate('text',label='F2',x =0, y=0.6)+
  geom_segment(mapping=aes(x=0, y=0*tan(pi*70/180), xend=.2, yend=.2*tan(pi*70/180)), arrow=arrow(length=unit(0.4,"cm")), size=.5, color="black",linetype=2)+
  geom_segment(mapping=aes(x=0, y=-0*tan(pi*20/180), xend=0.85, yend=-0.85*tan(pi*20/180)), arrow=arrow(length=unit(0.4,"cm")), size=.5, color="black",linetype=2)+
  annotate('text',label='1',x =0.553+0.05,y=0.429)+
  annotate('text',label='2',x =0.568+0.05,y=0.288)+
  annotate('text',label='3',x =0.392+0.05,y=0.450)+
  annotate('text',label='4',x =0.740+0.05,y=-0.273)+
  annotate('text',label='5',x =0.724+0.05,y=-0.211)+
  annotate('text',label='6',x =0.595+0.05,y=-0.132)+
  annotate('text',label='F2*',x =0.2+.05,y=0.2*tan(pi*70/180))+
  annotate('text',label='F1*',x =0.85+.05,y=-0.85*tan(pi*20/180))+
  annotate('text',label='0',x =0,y=-0.03)+  
  annotate('text',label='1',x =1,y=-0.03)+
  annotate('text',label='0.5',x =0.5 ,y=-0.03)+
  annotate('text',label='0.5',x =.03,y=.5)+
  annotate('text',label='-0.5',x =.03,y=-.5)+
  theme(axis.title.x = element_blank(),axis.text.y = element_blank(),axis.title.y = element_blank(),axis.text.x = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.background = element_blank())

# 方差最大化的旋转: varimax()
T.fit = varimax(A, normalize=TRUE)      
theta = acos(T.fit$rotmat[1,1])*180/pi                                      ## 旋转角度  
fcv = factanal(factors = 2,covmat = R.score, rotation="varimax", n.obs=220) ## 方差最大化旋转

#方差最大旋转直角坐标图，最大旋转角度：32.6度
x = A[,1];  y = A[,2]
data = data.frame(x, y, ID=1:6)

ggplot(data, aes(x=x, y=y)) +   
  geom_point(size=2)+
  coord_cartesian(xlim=c(0, 1.2),ylim=c(-0.6,0.6))+
  geom_segment(mapping=aes(x=0, y=-0.6, xend=0, yend=0.55), arrow=arrow(length=unit(0.4,"cm")), size=.5, color="black",linetype=1)+
  geom_segment(mapping=aes(x=0, y=0, xend=1.1, yend=0), arrow=arrow(length=unit(0.4,"cm")), size=.5, color="black",linetype=1)+
  annotate('text',label='F1',x = 1.15,y=0)+
  annotate('text',label='F2',x =0, y=0.6)+
  geom_segment(mapping=aes(x=0, y=0*tan(pi*57/180), xend=.35, yend=.35*tan(pi*57/180)), arrow=arrow(length=unit(0.4,"cm")), size=.5, color="black",linetype=2)+
  geom_segment(mapping=aes(x=0, y=-0*tan(pi*33/180), xend=0.8, yend=-0.8*tan(pi*33/180)), arrow=arrow(length=unit(0.4,"cm")), size=.5, color="black",linetype=2)+
  annotate('text',label='1',x =0.553+0.05,y=0.429)+
  annotate('text',label='2',x =0.568+0.05,y=0.288)+
  annotate('text',label='3',x =0.392+0.05,y=0.450)+
  annotate('text',label='4',x =0.740+0.05,y=-0.273)+
  annotate('text',label='5',x =0.724+0.05,y=-0.211)+
  annotate('text',label='6',x =0.595+0.05,y=-0.132)+
  annotate('text',label='F2*',x =0.35+.05,y=0.35*tan(pi*57/180))+
  annotate('text',label='F1*',x =0.8+.05,y=-0.8*tan(pi*33/180))+
  annotate('text',label='0',x =0,y=-0.03)+
  annotate('text',label='1',x =1,y=-0.03)+
  annotate('text',label='0.5',x =0.5 ,y=-0.03)+
  annotate('text',label='0.5',x =.03,y=.5)+
  annotate('text',label='-0.5',x =.03,y=-.5)+
  theme(axis.title.x = element_blank(),axis.text.y = element_blank(),axis.title.y = element_blank(),axis.text.x = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.background = element_blank())



####################
# Exam 7: 因子个数确定
# Likelihood ratio test 检验
######################
factanal(covmat=R, factors=1, rotation="none", n.obs=696)
factanal(covmat=R, factors=2, rotation="none", n.obs=696)

#平行分析方法(Parallel Analysis)
library(psych)  
fa.parallel(R, n.obs=696) 
fa.parallel(R, n.obs=696, fa=“fa”) 
fa.parallel(R, n.obs=696, fa=“pc”)



####################
# Exam 8：31个国家/地区预期寿命数据分析
######################

library(ggplot2)
library(GGally)
library(psych) 

life.data <- read.table("life.txt") 

life <- structure(.Data=life.data[,2:9], class = "data.frame",
                 names = c("m0", "m25", "m50", "m75", "w0", "w25", "w50", "w75"), 
                 row.names = life.data[,1])
# 计算相关系数矩阵
R <- cor(life)
R

pairs(life)

# 绘制碎石图，确定因子个数k
x <- scale(life, center = T, scale = T)
# k = fa.parallel(x, fa = "fa")
x
factanal(life, factors=2, rotation="varimax")
factanal(life, factors=3, rotation="varimax")

#因子得分
life.Thompson <- factanal(life, factors=3, rotation="varimax", scores="regression")$scores
life.Thompson

life.Bartlett <- factanal(life, factors=3, rotation="varimax", scores="Bartlett")$scores
life.Bartlett


#使用3-D图绘制此数据集的3个因子得分
# 使用原始数据和分数创建数据框
library(lattice)
life.df <- data.frame(life, life.Thompson)
attach(life.df) 
cloud(Factor3 ~ Factor1 * Factor2, xlim=range(Factor1), ylim=range(Factor2), zlim=range(Factor3),pch=row.names(life.df),scales = list(distance = rep(1, 3), arrows = FALSE))

row.names(life.df)[order(Factor1)]

row.names(life.df)[order(Factor2)]

row.names(life.df)[order(Factor3)]


# 进行因子得分的二维散点图
# Factor 2 vs. Factor 1
plot(Factor1, Factor2, type='n', xlab='Factor 1 scores', ylab='Factor 2 scores')
text(Factor1, Factor2, labels = row.names(life.df), cex = 0.7)
abline(h=0,lty=2)
abline(v=0,lty=2)

#Factor 3 vs. Factor 1
plot(Factor1, Factor3, type='n', xlab='Factor 1 scores', ylab='Factor 3 scores')
text(Factor1, Factor3, labels = row.names(life.df), cex = 0.7)
abline(h=0,lty=2)
abline(v=0,lty=2)

# Factor 3 vs. Factor 2
plot(Factor2, Factor3, type='n', xlab='Factor 2 scores', ylab='Factor 3 scores')
text(Factor2, Factor3, labels = row.names(life.df), cex = 0.7)
abline(h=0,lty=2)
abline(v=0,lty=2)
