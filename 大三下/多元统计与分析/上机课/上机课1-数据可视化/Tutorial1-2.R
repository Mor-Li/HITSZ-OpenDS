#######
# Chap 2

####################
# Exam 1
# scatter plot, bar plot, box plot and ridged plot
######################

# change the directory first, change Chinese to English
Consumer <- read.csv("consumer2018en.csv")
attach(Consumer)
par(mfrow=c(2,2),mai=c(0.6,0.6,0.4,0.4),cex=0.7,cex.main=1,font.main=1)
#plot(食品烟酒,衣着, main = "(a) 散点图")
plot(Eating,Cloth, main = "(a) Scatter Plot")

#plot(as.factor(三大地带), xlab="地带", col="yellow", main = "(b) 条形图")
plot(as.factor(District), xlab="District", col="yellow", main = "(b) Bar Plot")

#plot(居住~as.factor(区域划分), xlab="区域", col="yellow", main = "(c) 箱线图")
plot(Living~as.factor(Direction), xlab="Direction", col="yellow", main = "(c) Box Plot")

#plot(as.factor(三大地带)~as.factor(区域划分), xlab="区域", ylab="地带", main = "(d) 脊形图")
plot(as.factor(District)~as.factor(Direction), xlab="Direction", ylab="District", main = "(d) Ridged Plot")


####################
# Exam 2
# regression, Diagnostics
######################

par(mfrow=c(2,2),mai=c(0.6,0.6,0.4,0.4),cex=0.6)
lm.fit = lm(Eating~Cloth)
plot(lm.fit)


####################
# Exam 3
# Plot
######################

#(mai = c(0.7,0.7,0.4,0.4),cex = 0.8)
d <- read.csv("Income1.csv",header = TRUE)

plot(d$x,d$y,xlab = "EduYear",ylab = "Income",pch = 19,cex = 1.3,col = 'red')
grid(col = "grey60")                                                                        #添加网格线
axis(side = 4,lty = 1)                                                                      #添加坐标轴
points(mean(d$x),mean(d$y),pch = 19,cex = 4,col = 'black')                                  #添加均值点
abline(v = mean(d$x),h = mean(d$y),lty = 2,col = "gray30")                                  #添加均值垂直线和水平线
abline(lm(d$y~d$x),lwd = 2,col = 'blue')                                                    #添加回归直线
fit = lm(d$y~d$x)                                                            
d$predicted = predict(fit)                                                                  #保存预测值
d$residuals = residuals(fit)                                                                #保存残差
segments(d$x,d$y,d$x,d$predicted)                                                           #添加实际值与预测值的连线
arrows(17,55,18.5,46,code = 2,angle = 25,length = 0.06,col = 'blue',lwd = 2)                #添加带箭头的线段
text(19,45,expression(hat(y) == hat(beta)[0]+hat(beta)[1]*x))                               #添加文本
arrows(17.9,65,15,70,code = 2,angle = 25,length = 0.06,col = 'black',lwd = 2)               #添加带箭头的线段
text(13.7,71,expression(hat(epsilon[i]) == y[i]-hat(beta)[0]+hat(beta)[1]*x[i]))            #添加文本
legend("topleft",legend = "Regression Line",lty = c(1,6),col = 'blue',
       cex = 0.95,fill = "blue",box.col = "grey60",ncol = 1,inset = 0.01,x.intersp = 0.3)   #添加图例
box(col = 1,lwd = 2)


####################
# Exam 4
# outline plot
# data: 2018年全国31个地区的8项人均消费
# Package: DescTools, Fun: PlotLinesA
######################
library(DescTools)
par(mai=c(0.6,0.6,0.6,0.6),cex=0.7,cex.main=1,font.main=1)
Consumer = read.csv("consumer2018en.csv")
attach(Consumer)
data.m = as.matrix(Consumer[,4:11])
rownames(data.m) = Consumer[,1]
PlotLinesA(t(data.m), xlab="Item", ylab="Expenditure(RMB Yuan)", args.legend=NA, col=rainbow(31), pch=21, pch.col=1,
           pch.bg="white", pch.cex=1)
legend(x="topright", legend=Consumer[,1], lty=1, col=rainbow(31), 
       box.col="grey1", inset=0.01, ncol=4, cex=0.8)


# 轮廓图：2018年全国31个地区的8项人均消费
# Package: GGally, Fun: ggparcoord
library(ggplot2); library(GGally)
ggparcoord(Consumer,columns = 4:11, groupColumn = 1, scale = "globalminmax", showPoints = TRUE)+
  theme_bw()+                                          #设置图形主题
  theme(legend.text = element_text(size = "10"),       #设置图例字体大小
        axis.text = element_text(size = 10))+          #设置坐标轴字体大小
  labs(x = "Item", y = "Expenditure(RMB Yuan)")

# 轮廓图：2018年全国31个地区的8项人均消费
# Package: plotrix, Fun: ladderplot
library(plotrix)
par(mai=c(0.5,0.5,0.5,0.5),cex=1)
data.m = as.matrix(Consumer[,4:11])
rownames(data.m) = Consumer[,1]
ladderplot(data.m, col=rainbow(31), pch=21, lty=1)
legend(x="topright", legend=Consumer[,1], lty=1, col=rainbow(31), 
       box.col="grey1", inset=0.01, ncol=4, cex=0.8)


####################
# Exam 5
# Radar Chart
# 需要安装 4.2.2 版本的R
######################
library(ggplot2)
library(ggiraphExtra)
ggRadar(data = Consumer, aes(group = 地区), alpha = 0)+
  theme(axis.text = element_text(size = 10),
        legend.position = "right",                  #设置图例位置
        legend.text = element_text(size = "10"))    #设置图例字体大小


####################
# Exam 6
# Star Plot
######################
data.m <- as.matrix(Consumer[,4:11])
rownames(data.m) <- Consumer[,1]
stars(data.m,
      draw.segments = T,           #绘制线段图
      key.loc = c(10.5,1.8,5),     #调整比例尺的坐标
      cex = 1.3,                   #设置标签字体大小
      mar = c(0.85,0.1,0.1,1))     #设置图形边界



####################
# Exam 7
# Face Plot
######################
library(aplpack)
rownames(data.m) = Consumer[,1]
faces(data.m,
      face.type = 1,              #绘制彩色脸谱图
      scale = TRUE)               #将变量标准化



####################
# Exam 8
# Scatter Plot
# 需要 4.2.2
######################
library(ggpubr)
ggscatter(data = Consumer, x = "Cloth", y = "Living",
          add = "reg.line", conf.int = TRUE)+                       #添加回归线和置信区间
  stat_regline_equation(label.x = 1700,label.y = 10700,size = 6)+   #设置回归方程位置坐标
  stat_cor(label.x = 1700,label.y = 11400,size = 6)+                #设置相关系数位置坐标
  theme_bw()+                                                       #设置图形主题
  theme(axis.title.x = element_text(size = 15),axis.title.y = element_text(size = 15))



####################
# Exam 9
# 矩阵散点图 (matrix scatter plot)
######################
library(GGally)
ggpairs(data = Consumer, columns = 4:11) 



####################
# Exam 10 
# 3D Plot
######################
library(MASS)
library(ade4)
library(scatterplot3d)
data(iris)

par(mar = c(0, 0, 0, 0))
pane1 <- function(X, Y) {
  XY <- cbind.data.frame(X, Y)
  s.class(XY, iris$Species, include.ori = F, add.p = T, clab = 1.5,
          col = c("blue", "black", "red"), cpoi = 2, csta = 0.5)
}
pairs(iris[, 1:4], panel = pane1)

detach(package:ade4)

par(mfrow = c(2, 2)); mar0 = c(3, 3, 1, 3)
scatterplot3d(iris[, 1], iris[, 2], iris[, 3], mar = mar0, 
              color = c("blue","black", "red")[iris$Species], pch = 19,xlab = "萼片长度",ylab = "萼片宽度",zlab = "花瓣长度")
scatterplot3d(iris[, 2], iris[, 3], iris[, 4], mar = mar0, 
              color = c("blue","black", "red")[iris$Species], pch = 19,xlab = "萼片宽度",ylab = "花瓣长度",zlab = "花瓣宽度")
scatterplot3d(iris[, 3], iris[, 4], iris[, 1], mar = mar0, 
              color = c("blue","black", "red")[iris$Species], pch = 19,xlab = "花瓣长度",ylab = "花瓣宽度",zlab = "萼片长度")
scatterplot3d(iris[, 4], iris[, 1], iris[, 2], mar = mar0, 
              color = c("blue","black", "red")[iris$Species], pch = 19,xlab = "花瓣宽度",ylab = "萼片长度",zlab = "萼片宽度")
detach(package:scatterplot3d)



####################
# Exam 11
# 调和曲线图
######################
library(MSG)
andrews_curve(data.m)
grid(col = "grey60")                                                                   #添加网格线
legend(x = "topright",legend = Consumer[,1],lty = 1,lwd = 1,seg.len = 0.5,col = rainbow(31),
       box.col = "black",inset = 0.01,ncol = 3,cex = 1.43,x.intersp = 0.01,
       y.intersp = 0.3,text.width = 0.1)                                               #添加图例



####################
# Exam 12
# 气泡图 (bubble chart)
######################
library(DescTools)
cols = c("olivedrab1","orange","green","mediumturquoise","mediumorchid2","firebrick1")
PlotBubble(x = state.x77[,"Income"], y = state.x77[,"Life Exp"], cex=.00004,
           area = state.x77[,"Population"], col = cols[state.region], border="grey50",
           panel.first=grid(), xlab="Income", ylab="Life Exp.", las=1
)

BubbleLegend(x = "topright", area = c(20000, 10000, 1000), cex=.00004, frame=NA,
             cols=cols[1:3], labels = c(20000, 10000, 1000), cex.names=0.7)

legend(x="bottomright", fill=cols[1:4], legend=levels(state.region))


demo(graphics)



example(plotmath)




####################
# Exam 13
# Height~Weight data for 19 students
######################
Data2.1 <- read.table("Data2.1.txt", head=T)
attach(Data2.1)
par(mfrow=c(2,2),mai=c(0.6,0.6,0.4,0.4),cex=0.7,cex.main=1,font.main=1)
plot(Height,Weight, main = "(a) 散点图")
plot(as.factor(Sex), xlab="性别", main = "(b) 条形图")
plot(Weight~as.factor(Sex), xlab="性别", main = "(c) 箱线图")
plot(as.factor(Sex)~Weight, ylab="性别", main = "(d) 脊形图")




####################
# Exam 14
# 
######################
cl <- read.table("Data2.1.txt", head=T);
cl

names(cl) 
attach(cl)        ### 引用数据cl
plot(Height)      ### Height的散点图
plot(cl$Height)
coplot(Weight ~ Height | Sex) 
plot(Height, Weight, main="Regression on Height and Weight", xlab="Height", ylab="Weight") 


####################
# Exam 15-21
# 
######################
x = (1:50)/50
y = log(x)
plot(x, y, type="l") 

stem(cl$Weight, scale = 1, width = 80, atom = 1e-08)
stem(cl$Weight, scale = 2, width = 80, atom = 1e-08)
sort(Weight)

boxplot(cl$Weight) 
boxplot(cl$Height~cl$Sex,data=cl,col = "red")
boxplot(len ~ dose, data = ToothGrowth,
        boxwex = 0.25, at = 1:3 - 0.2,
        subset = supp == "VC", col = "yellow",
        main = "Guinea Pigs' Tooth Growth",
        xlab = "Vitamin C dose mg",
        ylab = "tooth length",
        xlim = c(0.5, 3.5), ylim = c(0, 35), yaxs = "i")

par(mfrow=c(2,1))
data(faithful) # read in data set 
names(faithful) 
attach(faithful) # to access the names above
boxplot(waiting,main="Waiting time",horizontal=TRUE)
boxplot(eruptions,main="Eruptions",horizontal=TRUE)

detach(faithful) # tidy up
par(mfrow=c(1,1))
par(mfrow=c(2,1))
data(faithful) # read in data set 
names(faithful) 

attach(faithful) # to access the names above
boxplot(waiting,main="Waiting time",horizontal=TRUE)
boxplot(eruptions,main="Eruptions",horizontal=TRUE)

detach(faithful) # tidy up
par(mfrow=c(1,1))
data(ToothGrowth)
boxplot(len ~ dose, data = ToothGrowth, boxwex = 0.25, at = 1:3 - 0.2, subset= supp == "VC", col="yellow", main="Guinea Pigs' Tooth Growth", xlab="Vitamin C dose mg", ylab="tooth length", ylim=c(0,35))
boxplot(len ~ dose, data = ToothGrowth, add = TRUE, boxwex = 0.25, at = 1:3 + 0.2, subset= supp == "OJ", col="orange") 
legend(2, 9, c("Ascorbic acid", "Orange juice"), fill = c("yellow", "orange"))


hist(cl$Weight, main = "Histogram of Weight", xlab = "Weight")
norm_x = rnorm(1000)
hist(norm_x)


qqnorm(Weight)
norm_x = rnorm(1000)
qqnorm(norm_x)
qqline(norm_x)



qqplot(Weight, Height) 
norm_x = rnorm(1000); norm_y = rnorm(500)
qqplot(norm_x, norm_y)


pairs(USJudgeRatings)
panel.hist = function(x, ...)
{
  usr = par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h = hist(x, plot = FALSE)
  breaks = h$breaks; nB = length(breaks)
  y = h$counts; y = y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col="cyan", ...)
}
pairs(USJudgeRatings[1:5], panel=panel.smooth,
      cex = 1.5, pch = 24, bg="light blue",
      diag.panel=panel.hist, cex.labels = 2, font.labels=2)

## put (absolute) correlations on the upper panels,
## with size proportional to the correlations.
panel.cor = function(x, y, digits=2, prefix="", cex.cor)
{
  usr = par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r = abs(cor(x, y))
  txt = format(c(r, 0.123456789), digits=digits)[1]
  txt = paste(prefix, txt, sep="")
  if(missing(cex.cor)) cex.cor = 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}
pairs(USJudgeRatings, lower.panel=panel.smooth, upper.panel=panel.cor)



####################
# Exam 22
# contour()函数画等值，persp()函数画三维图形。
######################

# EX 22.1
x <- seq(-10, 10, length= 30)
y <- x
f <- function(x,y) { 
  r <- sqrt(x^2+y^2); 10 * sin(r)/r }
z <- outer(x, y, f)
z[is.na(z)] = 1
persp(x, y, z, theta = 30, phi = 30, expand = 0.5, col = "lightblue")


# EX 22.2
x <- seq(0,2800, 400)
y <- seq(0,2400,400)
z <- scan()
1180 1320 1450 1420 1400 1300  700  900
1230 1390 1500 1500 1400  900 1100 1060
1270 1500 1200 1100 1350 1450 1200 1150
1370 1500 1200 1100 1550 1600 1550 1380
1460 1500 1550 1600 1550 1600 1600 1600
1450 1480 1500 1550 1510 1430 1300 1200
1430 1450 1470 1320 1280 1200 1080  940

# 需要在上述之后 console里敲一下回车

Z <- matrix(z, nrow=8)
image(x, y, Z)
contour(x, y, Z, levels = seq(min(z), max(z), by = 80))
persp(x, y, Z)


# Ex22.3
x = y = seq(-2*pi, 2*pi, pi/15)
f <- function(x,y) sin(x)*sin(y)
z <- outer(x,y, f)
contour(x,y,z,col="blue")

persp(x,y,z,theta=30, phi=30, expand=0.7,col="lightblue")


# EX22.4
data(volcano)
x = 10*(1:nrow(volcano))
y = 10*(1:ncol(volcano))
image(x, y, volcano, col = terrain.colors(100), axes = FALSE)
contour(x, y, volcano, levels = seq(90, 200, by=5), add = TRUE, col = "peru")
axis(1, at = seq(100, 800, by = 100))
axis(2, at = seq(100, 600, by = 100))
box()
title(main = "Maunga Whau Volcano", font.main = 4)

