% Compound trapezoid formula
clc
clear
syms x;
f=@(x) 1./(1+x.^2);
a=-2;
b=2;
n=20;
h=(b-a)/n;
sum=0;
for k=1:1:n-1
  x(k)=a+k*h;
  y(k)=f(x(k));
  sum=sum+y(k);
end
answer=h/2*(f(a)+f(b)+2*sum);
trapz_answer=eval(answer)
ground_truth=2*atan(2)
trapz_bias= abs(trapz_answer-ground_truth)

simpsons_answer = simpsons(f,a,b,n)
simpsons_bias = abs(simpsons_answer-ground_truth)

syms h;
% h=(b-a)/10;
sum=0;
for k=1:1:10-1
  x(k)=a+k*h;
  y(k)=f(x(k));
  sum=sum+y(k);
end
trapz_10_formula=h/2*(f(a)+f(b)+2*sum);
%在trapz_10_formula基础上再次二分之后得到的公式如下


sum=0;
for k=1:1:20-1
  x(k)=a+k*h/2;
  y(k)=f(x(k));
  sum=sum+y(k);
end
trapz_20_formula=h/4*(f(a)+f(b)+2*sum);
%里面的h仍为(b-a)/10


Romberg1_formula = trapz_20_formula*4/3-1/3*trapz_10_formula
Romberg1=eval(subs(Romberg1_formula,h,(b-a)/10))
Romberg1_bias = abs(Romberg1-ground_truth)
%上面的先分10份然后用龙贝格一次加速的任务已完成



syms h;
% h=(b-a)/5;
sum=0;
for k=1:1:5-1
  x(k)=a+k*h;
  y(k)=f(x(k));
  sum=sum+y(k);
end
trapz_5_formula=h/2*(f(a)+f(b)+2*sum);
%上面是分成五份的复合梯形公式。

syms h;
% h=(b-a)/5;
sum=0;
for k=1:1:10-1
  x(k)=a+k*h/2;
  y(k)=f(x(k));
  sum=sum+y(k);
end
trapz_10_formula=h/4*(f(a)+f(b)+2*sum);
%上面是分成十份的复合梯形公式。
%里面的h仍为(b-a)/5

syms h;
% h=(b-a)/5;
sum=0;
for k=1:1:20-1
  x(k)=a+k*h/4;
  y(k)=f(x(k));
  sum=sum+y(k);
end
trapz_20_formula=h/8*(f(a)+f(b)+2*sum);
%上面是分成二十份的复合梯形公式。
%里面的h仍为(b-a)/5

romberg21_formula=trapz_10_formula*4/3-1/3*trapz_5_formula;
romberg22_formula=trapz_20_formula*4/3-1/3*trapz_10_formula;
%上面分别进行了第一次龙贝格加速

romberg2_formula=romberg22_formula*16/15-1/15*romberg21_formula
%这里进行第二次龙贝格加速

romberg2=eval(subs(romberg2_formula,h,(b-a)/5))
romberg2_bias = abs(romberg2-ground_truth)



%下面使用高斯勒让德积分公式

a = linspace(-2,2,11);
%matlab的数组下标从1开始
sum=0;
for i = 1:10
    sum = sum+Gauss_Legendre_quadrature(f,a(i),a(i+1),2);
end
% 复合的 2 点高斯公式
gauss_2points = sum
gauss_2points_bias= abs(gauss_2points-ground_truth)
a = linspace(-2,2,6);
%matlab的数组下标从1开始
sum=0;
for i = 1:5
    sum = sum+Gauss_Legendre_quadrature(f,a(i),a(i+1),4);
end
% 复合的 4 点高斯公式
gauss_4points = sum
gauss_4points_bias= abs(gauss_4points-ground_truth)

digits(100)
%下面是选做题：
%先验证复合三点高斯公式收敛阶数
for k=2:8
    sum=0;
    a = linspace(-2,2,2^k+1);
    for i = 1:2^k
        sum = sum+vpa(Gauss_Legendre_quadrature(f,a(i),a(i+1),3),100);
    end
    gauss_3points_bias(k)=abs(sum-ground_truth)
end

for k=2:7
    a = log(gauss_3points_bias(k+1))-log(gauss_3points_bias(k));
    b = log(4/(2^(k+1)))-log(4/(2^(k)));
    order_gauss = a/b
end
%此后的结果趋向于大概是6，说明复合三点高斯公式余项阶数是6


%验证复合梯形公式收敛阶数
for m=2:6
    % Compound trapezoid formula
    b=2;
    a=-2;
    n=2^m;
    h=(b-a)/n;
    sum=0;
    for k=1:1:n-1
      x(k)=a+k*h;
      y(k)=f(x(k));
      sum=sum+y(k);
    end
    answer=h/2*(f(a)+f(b)+2*sum);
    trapz_answer=eval(answer);
    ground_truth=2*atan(2);
    trapz_bias(m)= abs(trapz_answer-ground_truth);
end
trapz_bias
for k=2:5
    a = log(trapz_bias(k+1))-log(trapz_bias(k));
    b = log(4/(2^(k+1)))-log(4/(2^(k)));
    order_trapz = a/b
end
%此后的结果趋向于大概是2，说明复合梯形公式余项阶数是2


%验证复合辛普森公式收敛阶数
for m=2:6
    a=-2;
    b=2;
    simpsons_answer = simpsons(f,a,b,2^m);
    simpsons_bias(m) = abs(simpsons_answer-ground_truth);
end
simpsons_bias
for k=2:5
    a = log(simpsons_bias(k+1))-log(simpsons_bias(k));
    b = log(4/(2^(k+1)))-log(4/(2^(k)));
    order_simpson = a/b
end
%此后的结果趋向于大概是4，说明复合辛普森公式余项阶数是4

