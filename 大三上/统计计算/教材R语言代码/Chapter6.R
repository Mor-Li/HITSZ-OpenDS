#######################################################################
#  Chapter 6
####################################################################

###Ex6.1.1
set.seed(1)
n=100; mu=0; sigma=2; m=2000;
X=rnorm(n,mu,sigma);
sigma2hat=mean((X-mean(X))^2) #基于样本信息，sigma^2的估计
sigma2Bst=NULL
for (i in (1:m)){
  O=sample(seq(1,n),n,replace=T)
  Bstrap_X=X[O]          #有放回的从X中抽取新样本
  sigma2Bst[i]=mean((Bstrap_X-mean(Bstrap_X))^2) # 基于新样本，sigma^2的估计
}
Bias=mean(sigma2Bst)-sigma2hat    #偏差的Bootstrap 估计


###Ex6.1.2
set.seed(1)
library(mnormt)
n=100;
mu1=mu2=0
sigma1=1
sigma2=2
rho=0.6
sigma12=sigma21=rho*sigma1*sigma2
mu=c(mu1,mu2)
Sigma=matrix(c(sigma1^2,sigma12,sigma21,sigma2^2),2,2)
Data=rmnorm(n,mu,Sigma)
X=Data[,1]
Y=Data[,2]
  #基于样本信息，rho的估计
rhohat=sum((X-mean(X))*(Y-mean(Y)))/sqrt(sum((X-mean(X))^2)*sum((Y-mean(Y))^2))
m=2000
rhoBst=NULL
for (i in (1:m)){
  O=sample(seq(1,n),n,replace=T)
  Bst_Data=Data[O,]      #有放回的从X中抽取新样本
  X=Bst_Data[,1]
  Y=Bst_Data[,2]
   #基于新样本，rho的估计
  rhoBst[i]=sum((X-mean(X))*(Y-mean(Y)))/sqrt(sum((X-mean(X))^2)*sum((Y-mean(Y))^2))
}
Bias=mean(rhoBst)-rhohat    #偏差的Bootstrap估计

###Ex6.1.3
set.seed(1)
n1=100
n2=200
mu1=0
mu2=0.3
sigma1=1
sigma2=2
X=rnorm(n1,mu1,sigma1)
Y=rnorm(n2,mu2,sigma2)
diffhat=mean(X)-mean(Y)      #基于样本信息,均值差的估计
m=2000 
diffBst=NULL
for (i in (1:m)){
  O1=sample(seq(1,n1),n1,replace=T)
  O2=sample(seq(1,n2),n2,replace=T)
  Bst_X=X[O1]
  Bst_Y=Y[O2]     #分别有放回的从X,Y中抽取新样本
  diffBst[i]=mean(Bst_X)-mean(Bst_Y)     #基于新样本,均值差的估计
}
Bias=mean(diffBst)-diffhat    #偏差的Bootstrap估计


###Ex6.1.4
set.seed(1)
n=100
mu=0
sigma=2
m=2000
X=rnorm(n,mu,sigma)
muhat=mean(X)    #基于样本信息，mu的估计
muBst=NULL
for (i in (1:m)){
  O=sample(seq(1,n),n,replace=T)
  Bstrap_X=X[O]       #有放回的从X中抽取新样本
  muBst[i]=mean(Bstrap_X)     #基于新样本,mu的估计
}
var_muBst=mean((muBst-mean(muBst))^2)    #方差的Bootstrap估计


###Ex6.1.5
set.seed(1)
library(mnormt)
n=100
mu1=mu2=0
sigma1=1
sigma2=2
rho=0.6
sigma12=sigma21=rho*sigma1*sigma2
mu=c(mu1,mu2)
Sigma=matrix(c(sigma1^2,sigma12,sigma21,sigma2^2),2,2)
Data=rmnorm(n,mu,Sigma)
X=Data[,1]
Y=Data[,2]
   #基于样本信息，rho的估计
rhohat=sum((X-mean(X))*(Y-mean(Y)))/sqrt(sum((X-mean(X))^2)*sum((Y-mean(Y))^2));
m=2000
rhoBst=NULL
for (i in (1:m)){
  O=sample(seq(1,n),n,replace=T);
  Bst_Data=Data[O,]     #有放回的抽取新样本
  X=Bst_Data[,1]
  Y=Bst_Data[,2]
   #基于新样本,rho的估计 
  rhoBst[i]=sum((X-mean(X))*(Y-mean(Y)))/sqrt(sum((X-mean(X))^2)*sum((Y-mean(Y))^2))
}
var_rhoBst=mean((rhoBst-mean(rhoBst))^2)    #方差的Bootstrap估计


###Ex6.1.6
set.seed(1)
n1=100
n2=200
mu1=0
mu2=0.2
sigma1=1
sigma2=2
X=rnorm(n1,mu1,sigma1)
Y=rnorm(n2,mu2,sigma2)
Sx=sum((X-mean(X))^2)/(n1-1)
Sy=sum((Y-mean(Y))^2)/(n2-1)
ratiohat=Sx/Sy   #基于样本信息，方差比值的估计
m=2000
ratioBst=NULL
for (i in (1:m)){
  O1=sample(seq(1,n1),n1,replace=T)
  O2=sample(seq(1,n2),n2,replace=T)
  X_Bst=X[O1]
  Y_Bst=Y[O2]      #分别有放回的从X,Y抽取新样本
  Sx_Bst=sum((X_Bst-mean(X_Bst))^2)/(n1-1)
  Sy_Bst=sum((Y_Bst-mean(Y_Bst))^2)/(n2-1)
  ratioBst[i]=Sx_Bst/Sy_Bst    #基于新样本,方差比值的估计
}
var_ratioBst=mean((ratioBst-mean(ratioBst))^2)  #估计量方差的Bootstrap估计


###Ex6.2.1  方差估计偏差的Jackknife估计
set.seed(1)
n=100
mu=0
sigma=2
X=rnorm(n,mu,sigma)
sigma2hat=sum((X-mean(X))^2)/n    #基于原样本，sigma^2的估计
sigma2Jack=NULL
for (i in (1:n)){
  Jack_X=X[-i]      #去掉第b个样本点
  sigma2Jack[i]=sum((Jack_X-mean(Jack_X))^2)/(n-1)   # 基于新样本，sigma^2的估计
}
Bias=(n-1)*mean(sigma2Jack-sigma2hat)    #偏差的Jackknife估计



###Ex6.2.2  相关系数偏差的Jackknife估计
set.seed(1)
library(mnormt)
n=100
mu1=mu2=0
sigma1=1
sigma2=2
rho=0.6
sigma12=sigma21=rho*sigma1*sigma2
mu=c(mu1,mu2)
Sigma=matrix(c(sigma1^2,sigma12,sigma21,sigma2^2),2,2)
Data=rmnorm(n,mu,Sigma)
X=Data[,1]
Y=Data[,2]
  #基于原样本，rho的估计
rhohat=sum((X-mean(X))*(Y-mean(Y)))/sqrt(sum((X-mean(X))^2)*sum((Y-mean(Y))^2))
rhoJack=NULL
for (i in (1:n)){
  Jack_Data=Data[-i,]       #去掉第b组（X,Y)
  X=Jack_Data[,1]
  Y=Jack_Data[,2]
   #基于新样本，rho的估计
  rhoJack[i]=sum((X-mean(X))*(Y-mean(Y)))/sqrt(sum((X-mean(X))^2)*sum((Y-mean(Y))^2))
}
Bias=(n-1)*mean(rhoJack-rhohat)    #偏差的Jackknife估计


###Ex6.2.3  课本题目错了，PPT也错了，是关于mu的方差的Jackknife估计
set.seed(1)
n=100
mu=0
sigma=2
X=rnorm(n,mu,sigma)
muhat=mean(X)       #基于样本信息，mu的估计
muJack=NULL
for (i in (1:n)){
  Jack_X=X[-i]
  muJack[i]=mean(Jack_X)     #基于新样本，mu的估计
}
Varhat=(n-1)*mean((muJack-mean(muJack))^2)    #方差Jackknife估计


###Ex6.2.4  相关系数的Jackknife估计，及其方差
set.seed(1)
library(mnormt)
n=100
mu1=mu2=0
sigma1=1
sigma2=2
rho=0.6
sigma12=sigma21=rho*sigma1*sigma2
mu=c(mu1,mu2)
Sigma=matrix(c(sigma1^2,sigma12,sigma21,sigma2^2),2,2)
Data=rmnorm(n,mu,Sigma)
X=Data[,1]
Y=Data[,2]
   #基于样本信息，rho的估计
rhohat=sum((X-mean(X))*(Y-mean(Y)))/ sqrt(sum((X-mean(X))^2)*sum((Y-mean(Y))^2))
rhoJack=NULL
for (i in (1:n)){
  Jack_Data=Data[-i,]
  X=Jack_Data[,1]
  Y=Jack_Data[,2]
  #基于新样本，rho的估计
  rhoJack[i]=sum((X-mean(X))*(Y-mean(Y)))/sqrt(sum((X-mean(X))^2)*sum((Y-mean(Y))^2))
}
Varhat=(n-1)*mean((rhoJack-mean(rhoJack))^2)

###Ex6.2.5  Jackknife after Bootstrap  样本均值
set.seed(1)
n=100
mu=0
sigma=2
X=rnorm(n,mu,sigma)
m=2000
muBst=var_muBst=NULL
for (i in (1:n)){
  X_Jack=X[-i]
  for (j in (1:m)){
    O=sample(seq(1,n-1),n-1,replace=T)
    X_Jack_Bstrap=X_Jack[O] #去掉第i个样本后，有放回抽样得到的新样本
    muBst[j]=mean(X_Jack_Bstrap) #基于新样本，得到的mu的估计
  }
  var_muBst[i]=mean((muBst-mean(muBst))^2)  #Bootstrap 方法得到的方差估计
}
varJack_muBst=(n-1)*mean((var_muBst-mean(var_muBst))^2)
#Jackknife方法得到的Bootstrap估计量的方差



###Ex6.2.6  Jackknife after Bootstrap  相关系数
set.seed(1)
library(mnormt)
n=100
mu1=mu2=0
sigma1=1
sigma2=2
rho=0.6
sigma12=sigma21=rho*sigma1*sigma2
mu=c(mu1,mu2)
Sigma=matrix(c(sigma1^2,sigma12,sigma21,sigma2^2),2,2)
Data=rmnorm(n,mu,Sigma)
X=Data[,1]
Y=Data[,2]
#基于样本信息，rho的估计
rhohat=sum((X-mean(X))*(Y-mean(Y)))/sqrt(sum((X-mean(X))^2)*sum((Y-mean(Y))^2))
m=2000
rhoBst=var_rhoBst=NULL
for (i in (1:n)){
  Data_Jack=Data[-i,]
  for (j in (1:m)){
    O=sample(seq(1,n-1),n-1,replace=T)
    Data_Jack_Bst=Data_Jack[O,]
    X=Data_Jack_Bst[,1]
    Y=Data_Jack_Bst[,2]
      #rho的Bootstrap估计
    rhoBst[j]=sum((X-mean(X))*(Y-mean(Y)))/sqrt(sum((X-mean(X))^2)*sum((Y-mean(Y))^2))
  }
  var_rhoBst[i]=mean((rhoBst-mean(rhoBst))^2)   #Bootstrap 方法得到的方差估计
}
varJack_rhoBst=(n-1)*mean((var_rhoBst-mean(var_rhoBst))^2)
#Jackknife方法得到的Bootstrap估计量的方差


###Ex6.3.1  Bootstrap  置信区间
set.seed(1)
alpha=0.05
n=100
mu=0
sigma=1
X=rnorm(n,mu,sigma)
muhat=mean(X)         #基于样本信息，mu的估计
m=K=rep_muBst=500
muBst=t=NULL
for (i in (1:m)){
  O=sample(seq(1,n),n,replace=T)
  Bstrap_X=X[O]         #有放回的从X中抽取样本
  muBst[i]=mean(Bstrap_X) #基于重抽样样本，mu的估计
  for (k in (1:K)){
    O=sample(seq(1,n),n,replace=T)
    rep_Bstrap_X=Bstrap_X[O]     #有放回的从重抽样样本中抽取样本
    rep_muBst[k]=mean(rep_Bstrap_X)
  }
  se_rep=sqrt(mean((rep_muBst-mean(rep_muBst))^2)) #基于重抽样样本估计的估计量标准差
  t[i]=(muBst[i]-muhat)/se_rep   #基于第i组重抽样样本和估计的标准差，构建t统计量
}
#构造第一种置信区间和第四种置信区间需要的标准差估计
se_muBst=sqrt(mean((muBst-mean(muBst))^2))
L1=muhat-qnorm(1-alpha/2,0,1)*se_muBst
U1=muhat+qnorm(1-alpha/2,0,1)*se_muBst   #第一种区间
smuBst=sort(muBst)
L2=smuBst[round(m*alpha/2)]
U2=smuBst[round(m*(1-alpha/2))]       #第二种区间
L3=2*muhat-smuBst[round(m*(1-alpha/2))]
U3=2*muhat-smuBst[round(m*alpha/2)]    #第三种区间
st=sort(t)
L4=muhat-st[round(m*(1-alpha/2))]*se_muBst
U4=muhat-st[round(m*alpha/2)]*se_muBst    #第四种区间
c(L1,U1)
c(L2,U2)
c(L3,U3)
c(L4,U4)