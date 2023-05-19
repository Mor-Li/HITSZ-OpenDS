% 下面这里是lagrange插值插值
clc
%任务1.1
% 需要插值的点
x=linspace(-5,5,11);   
y=1./(1+x.^2);
%根据插值点构造插值多项式
xx=-5:0.01:5;
yy = lagrange(xx,x,y);
% 与真实函数图像做对比，f是真实函数图像。
f=1./(1+xx.^2);
plot(x,y,'o',xx,yy,xx,f,'--');
legend('插值点','lagrange插值多项式','被插值函数');
saveas(gcf,'lagrange.svg');
% 1.画出每个插值点，形式为圆圈
% 2.画出插值函数生成的图
% 3.画出真实函数图像以做对比

%误差计算
xxx=linspace(-5,5,101);
yyy = lagrange(xxx,x,y);
real_values=1./(1+xxx.^2);
abs(yyy-real_values);
bias=mean(abs(yyy-real_values))

clc
%任务1.2
% 需要插值的点
% find 11 Chebyshev nodes and mark them on the plot
n = 11;
k = 1:11; % iterator
xc = cos((2*k-1)/2/n*pi); % Chebyshev nodes
x=5.*xc;   
y=1./(1+x.^2);
%根据插值点构造插值多项式
xx=-5:0.01:5;
yy = lagrange(xx,x,y);
% 与真实函数图像做对比，f是真实函数图像。
f=1./(1+xx.^2);
plot(x,y,'o',xx,yy,xx,f,'--');
legend('插值点','chebyshev插值多项式','被插值函数');
saveas(gcf,'chebyshev.svg');
% 1.画出每个插值点，形式为圆圈
% 2.画出插值函数生成的图
% 3.画出真实函数图像以做对比

%误差计算
xxx=linspace(-5,5,101);
yyy = lagrange(xxx,x,y);
real_values=1./(1+xxx.^2);
abs(yyy-real_values);
bias=mean(abs(yyy-real_values))



clc
%任务1.3 （1）分段线性插值不调库法
syms x
y = 1/(1+x^2)
x0 = -5:5
y0 = 1./(1+x0.^2)
error = 0
for i = 1:10
    a=y0(i)*(x-x0(i+1))/(x0(i)-x0(i+1))+y0(i+1)*(x-x0(i))/(x0(i+1)-x0(i));
    for j = -6+i:0.1:-5+i
        error = error + abs(subs(a,x,j)-subs(y,x,j));
    end
    fplot(a,[-6+i,-5+i])
    hold on
end
a= eval(vpa(error,5))
fplot(y,[-5,5])
saveas(gcf,'another_linear.svg');
hold off



clc
%任务1.3 （2）分段线性插值调库法
% 需要插值的点
x=linspace(-5,5,11);   
y=1./(1+x.^2);
%根据插值点构造插值多项式
xx=-5:0.01:5;
yy = interp1(x,y,xx);
% 与真实函数图像做对比，f是真实函数图像。
f=1./(1+xx.^2);
plot(x,y,'o',xx,yy,xx,f,'--')
legend('插值点','线性插值多项式','被插值函数')
saveas(gcf,'linear.svg');
% 1.画出每个插值点，形式为圆圈
% 2.画出插值函数生成的图
% 3.画出真实函数图像以做对比

%误差计算
xxx=linspace(-5,5,101);
yyy = interp1(x,y,xxx);
real_values=1./(1+xxx.^2);
abs(yyy-real_values);
bias=mean(abs(yyy-real_values))






clc
%任务1.4 (1)三次样条插值调库法
% 需要插值的点
x=linspace(-5,5,11);   
y=1./(1+x.^2);

syms m
f = 1/(1+m^2);
diff_f = diff(f);
y1 = subs(diff_f,m,-5);
y2 = subs(diff_f,m,5);
y1=eval(y1);
y2=eval(y2);
y_spline=[y1 y y2];
%根据两端点的一阶导数值(clamped方法)
%即第一类边界条件
%根据插值点构造插值多项式
cs = spline(x,y_spline);

%计算三次样条插值各个密集点的值
xx=-5:0.01:5;
yy = ppval(cs,xx);
% 与真实函数图像做对比，f是真实函数图像。
f=1./(1+xx.^2);
plot(x,y,'o',xx,yy,xx,f,'--')
legend('插值点','三次样条插值多项式','被插值函数')
saveas(gcf,'spline.svg');
% 1.画出每个插值点，形式为圆圈
% 2.画出插值函数生成的图
% 3.画出真实函数图像以做对比

%误差计算
xxx=linspace(-5,5,101);
yyy = ppval(cs,xxx);
real_values=1./(1+xxx.^2);
abs(yyy-real_values);
bias=mean(abs(yyy-real_values))








clc
%任务1.4 (2)三次样条插值自编法
% 需要插值的点
x=linspace(-5,5,11);   
y=1./(1+x.^2);

syms m
f = 1/(1+m^2);
diff_f = diff(f);
y1 = subs(diff_f,m,-5);
y2 = subs(diff_f,m,5);
y1=eval(y1);
y2=eval(y2);
%根据两端点的一阶导数值(clamped方法)
%即第一类边界条件
%根据插值点构造插值多项式

%计算三次样条插值各个密集点的值
xx=-5:0.01:5;
yy = splineA(x,y,xx,[1,2],[y1,y2]);
% 与真实函数图像做对比，f是真实函数图像。
f=1./(1+xx.^2);
plot(x,y,'o',xx,yy,xx,f,'--')
legend('插值点','三次样条插值多项式','被插值函数')
saveas(gcf,'another_spline.svg');
% 1.画出每个插值点，形式为圆圈
% 2.画出插值函数生成的图
% 3.画出真实函数图像以做对比

%误差计算
xxx=linspace(-5,5,101);
yyy = ppval(cs,xxx);
real_values=1./(1+xxx.^2);
abs(yyy-real_values);
bias=mean(abs(yyy-real_values))











clc
%任务1.5 最佳平方逼近多项式
% 需要插值的点
x=linspace(-5,5,11);   
y=1./(1+x.^2);

syms m
legend_f=0;
f=1/(1+25*m^2);
for i = 0:4
    s=legendreP(i,m)*f;
    legend_f = legend_f+legendreP(i,m)*eval(int(s,m,-1,1))*(2*i+1)/2;
end
%计算勒让德多项式插值各个密集点的值
legend_f=subs(legend_f,m,m/5);
xx=-5:0.01:5;
yy = eval(subs(legend_f,m,xx));
% 与真实函数图像做对比，f是真实函数图像。
f=1./(1+xx.^2);
plot(x,y,'o',xx,yy,xx,f,'--')
legend('插值点','勒让德插值多项式','被插值函数')
saveas(gcf,'legendre.svg');
% 1.画出每个插值点，形式为圆圈
% 2.画出插值函数生成的图
% 3.画出真实函数图像以做对比

%误差计算
xxx=linspace(-5,5,101);
yyy = eval(subs(legend_f,m,xxx));
real_values=1./(1+xxx.^2);
abs(yyy-real_values);
bias=mean(abs(yyy-real_values))


clc 
%%%任务1.6 最小二乘拟合多项式
%% 需要插值的点
x_points=linspace(-5,5,11);   
y=1./(1+x_points.^2);
%根据插值点构造插值多项
syms x;
f = 1./(1+x.^2);
p = least_squares(f,x_points);
%此处由于不允许调库，所以改成手写
%如果调库，可以使用polyfit
xx=-5:0.01:5;
p4=subs(p,x,xx);
xxx=linspace(-5,5,101);
poly_vals=eval(subs(p,x,xxx));
%根据插值多项式生成一系列密集点用来画图

%与真实函数图像做对比，f是真实函数图像。
f=1./(1+xx.^2);
plot(x_points,y,'o',xx,p4,xx,f,'--');
legend('插值点','最小二乘拟合多项式','被插值函数');
saveas(gcf,'least_squares.svg');
%1.画出每个插值点，形式为圆圈
%2.画出插值函数生成的图
%3.画出真实函数图像以做对比

% 误差计算

real_values=1./(1+xxx.^2);
abs(poly_vals-real_values);
bias=mean(abs(poly_vals-real_values));