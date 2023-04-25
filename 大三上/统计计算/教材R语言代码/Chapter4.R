#######################################################################
#  Chapter 4
####################################################################

###Ex4.1.1
set.seed(1)
n=100
res=c()
mu=0.3
for (i in 1:1000) {
  data=rnorm(n)
  E_data=mean(data)+mu
  stat=E_data*sqrt(n)
  res[i]=as.numeric(abs(stat)>=qnorm(0.975,0,1))
}
result=mean(res)    #数值模拟估计的统计功效
criti=qnorm(0.975,0,1)
power=2-pnorm(criti-sqrt(n)*mu,0,1)-pnorm(criti+sqrt(n)*mu,0,1)    #统计功效
c(result, power)

###Ex4.1.2
set.seed(1)
n=100
m=50
sigma=2
res=c()
for (i in 1:1000) {
  X_data=rnorm(n)
  Y_data=sigma*rnorm(m)
  S_x=sum((X_data-mean(X_data))^2)/(n-1)
  S_y=sum((Y_data-mean(Y_data))^2)/(m-1)
  stat=S_x/S_y
  criti_1=qf(0.975,n-1,m-1)
  criti_2=qf(0.025,n-1,m-1)
  res[i]=as.numeric(stat>=criti_1)+as.numeric(stat<=criti_2)
}
result=mean(res)       #数值模拟估计的统计功效
power=1-pf(sigma^2*criti_1,n-1,m-1)+pf(sigma^2*criti_2,n-1,m-1)    #统计功效
c(result, power)



###Ex4.2.1
  #均匀性检验—卡方检验
set.seed(1)
m=10
inter=seq(0,1,by=1/m)
n=500
res1 = NULL
for (i in 1:1000){
  data=sqrt(runif(n,min = 0,max = 1))      # 备择假设成立
  # data=runif(n,min = 0,max = 1) # 原假设成立
  left_inter=rep(1,n)%*%t(inter[1:m])
  right_inter=rep(1,n)%*%t(inter[2:(m+1)])
  Data=(data)%*%t(rep(1,m))
  A = left_inter<=Data&Data<right_inter
  frequ = apply(A, 2, sum)
  stat1=m/n*sum((frequ-n/m)^2)
  res1[i]=stat1>qchisq(0.95,m-1)
}
result=mean(res1)


###Ex4.2.2  单样本KS检验

set.seed(1)
n=35
stat1 = NULL
res1 = NULL
res2 = NULL
for (i in 1:1000){
#data=rnorm(n,0,1) #数据来自原假设
data=rt(n,1) #数据来自备择假设
data=sort(data)
D_splus=max(abs(c(1:n)/n-pnorm(data)))
D_minus=max(abs(pnorm(data)-(c(1:n)-1)/n))
stat1=max(D_splus,D_minus)
res1[i]=as.numeric(stat1>0.23)
index=seq(1,10000,1)
p_val=2*sum((-1)^(index-1)*exp(-2*n*index^2*stat1^2))
res2[i]=as.numeric(p_val<0.05)
}
c(mean(res1),mean(res2))

###Ex4.2.3  变量值随机性检验--游程检验
set.seed(1)
n=200
B=c(1/6,5/24,11/120,19/720,29/5040,1/840)
A=matrix(data = c(4529.4,9044.9,13568,18091,22615,27892,0,18097,27139,
                  36187,45234,55789,0,0,40721,54281,67852,83685,0,0,0,
                  72414,90470,111580,0,0,0,0,113262,139476,0,0,0,0,0,
                  172860),byrow=T,nrow=6,ncol=6)
A=A+t(A)-diag(diag(A),nrow = 6,ncol = 6)
G = matrix(0,nrow = 1000,ncol = 6)
res = NULL
for (i in 1:1000) {
  data=runif(n,min = 0,max = 1)
  beg_stop=1
  for (j in 1:(n-1)) {
    if(data[j+1]<data[j])
      beg_stop=c(beg_stop,j+1)
  }
  beg_stop=c(beg_stop,n+1)
  end=length(beg_stop)
  runs = beg_stop[2:end]-beg_stop[1:(end-1)]     #确定游程长度
  G[i,]=c(sum(runs==1),sum(runs==2),sum(runs==3),
          sum(runs==4),sum(runs==5),sum(runs>=6))
  stat=t(G[i,]-n*B)%*%(A/n)%*%((G[i,]-n*B))
  res[i] = stat>=qchisq(0.95,6)
}
mean(res)


###Ex4.3.1  Wilcoxon秩和检验
set.seed(1)
m=50
n=80
resu = NULL
for (i in 1:1000){
  X_data=rnorm(n,0,1)
  Y_data=rnorm(m,0.3,1)
  Data=c(X_data,Y_data)
  Rank=rank(Data)
  W_x=sum(Rank[1:n])
  W_y=sum(Rank[(n+1):(n+m)])
  W_xy=(W_x-n*(n+1)/2)/(m*n)
  stat=(W_xy-1/2)/sqrt((m+n-1)/(12*m*n))
  resu[i]=as.numeric(abs(stat)>1.96)
}
mean(resu)  


###Ex4.3.2  同分布检验—K-S检验(柯氏检验)
set.seed(1)
m=50
n=30
N=m+n
stat1 = NULL
res1 = NULL
res2 = NULL
for (i in 1:1000){
  X_data=rt(n,3)
  Y_data=rchisq(m,1)-1
  F_nx=rowMeans((matrix(1,n,1)%*%X_data)<=(X_data%*%matrix(1,1,n)))
  F_ny=rowMeans((matrix(1,m,1)%*%X_data)<=(Y_data%*%matrix(1,1,n)))
  G_mx=rowMeans((matrix(1,n,1)%*%Y_data)<=(X_data%*%matrix(1,1,m)))
  G_my=rowMeans((matrix(1,m,1)%*%Y_data)<=(Y_data%*%matrix(1,1,m)))
  D_x=max(abs(F_nx-G_mx))
  D_y=max(abs(F_ny-G_my))
  stat1=max(D_x,D_y)
  res1[i]=as.numeric(stat1>=1.36*sqrt(N/(m*n)))     #确定临界值
  index=seq(1,10000,1)
  p_val=2*sum((-1)^(index-1)*exp(-2*m*n*index^2*stat1^2/N))
  res2[i]=as.numeric(p_val<0.05)   #判断p值是否小于犯一类错误的概率
}
c(mean(res1),mean(res2))


###Ex4.3.2  同分布检验—Wald-Wolfowitz游程检验
set.seed(1)
m=50
n=30
N=m+n
res1 = NULL
for (i in 1:1000){
  X_data=rt(n,3)
  Y_data=rchisq(m,1)-1
  # Y_data=rt(m,3) 原假设成立
  Data=c(X_data,Y_data)
  Runs=as.numeric(order(Data)<=n)
  W=1
  for (j in 1:(length(Runs)-1)){
    if (Runs[j]!=Runs[j+1])
      W=W+1
  }
  E=2*m*n/N+1;
  V=2*m*n*(2*m*n-N)/(N^2*(N-1))
  #resu[i]=(W-E)/sqrt(V)
  res1[i]=as.numeric(abs((W-E)/sqrt(V))>=1.96)
}
mean(res1)


###Ex4.4.1  独立检验--列联表检验
set.seed(1)
library(MASS)
k=8
n=100
Sigma = matrix(c(1,0.6,0.6,1),2,2)
res1 = NULL
for (i in 1:1000) {
  data=mvrnorm(n, rep(0, 2), Sigma)
  X_data=data[,1]
  Y_data=data[,2]
  Sx=sort(X_data)
  Sy=sort(Y_data)
  inter_x=seq(min(X_data),max(X_data), by=(max(X_data)-min(X_data))/k)
  inter_y=seq(min(Y_data),max(Y_data), by=(max(Y_data)-min(Y_data))/k)
  left_inter_x=rep(1,n)%*%t(inter_x[1:k])
  right_inter_x=rep(1,n)%*%t(inter_x[2:(k+1)])
  left_inter_y=rep(1,n)%*%t(inter_y[1:k])
  right_inter_y=rep(1,n)%*%t(inter_y[2:(k+1)])
  Data1=(X_data)%*%t(rep(1,k))
  Data2=(Y_data)%*%t(rep(1,k))
  frequx=(left_inter_x<=Data1)&(Data1<right_inter_x)
  frequy=(left_inter_y<=Data2)&(Data2<right_inter_y)
  frequxy = t(frequx)%*%frequy
  pi=apply(t(frequxy),2,sum)/n
  pj=apply(frequxy,2,sum)/n
  pij=pi%*%t(pj)
  A=(frequxy-n*pij)^2/(n*pij)
  A[is.nan(A)]=0
  stat1=sum(A)
  res1[i]=stat1>qchisq(0.95,(k-1)^2)
}
mean(res1)


###Ex4.4.2  独立检验--相关系数检验
set.seed(1)
n=100
beta1=0.5
beta2=-1
res1 = NULL
res2 = NULL
for (i in 1:1000){
  X_data=rnorm(n,0,1)
  epsilon=rnorm(n,0,1/sqrt(n))
  Y_data=beta1*X_data+beta2*X_data^2+epsilon
  R=rank(X_data)
  S=rank(Y_data)
  r_s=1-6*mean((R-S)^2)/(n^2-1)
  XX=X_data%*%t(rep(1,n))-rep(1,n)%*%t(X_data)
  YY=Y_data%*%t(rep(1,n))-rep(1,n)%*%t(Y_data)
  tau=sum(sign(XX*YY))/(n*(n-1))
  res1[i]=abs(r_s*sqrt(n-1))>1.96
  res2[i]=abs(tau*sqrt(9*n*(n-1)/(4*n+10)))>1.96
}
c(mean(res1),mean(res2))