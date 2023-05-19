function I = Gauss_Legendre_quadrature(f,a,b,point)
%{
How to use the program:
Suppose the function to be evaluated is:
f(x)= -0.0547x^4 +0.8646x^3 -4.1562x^2 +6.2917x +2
to be integrated from 0 to 8 

Enter the function to be integrated: 
(-0.0547*x^4)+(0.8646*x^3)-(4.1562*x^2)+(6.2917*x)+2

The lower limit is:0
The upper limit is:8

Choose what point Gauss-Legendre equation to use.(2 is min. and 6 is max):2
*If 2 is chosen, then a 2-point Gauss-Legendre equation would be used to
evaluate the integral of the given function; if 3, then 3 point, and so on.*
%}

%This is for inputs                                                               
%在-1到1基础上
a0=(b+a)/2;%中心点向右平移的长度
a1=(b-a)/2;%在长度上伸展的倍数

if point==2
    digits(100)
    x0=a0+(a1*(-1/(sqrt(3))));
    x1=a0+(a1*(1/(sqrt(3))));
    fx0=a1*f(x0);
    fx1=a1*f(x1);
    I=fx0+fx1;
end

if point==3
    digits(100)
    x0=a0+(a1*(-vpa(sqrt(3/5))));
    x1=a0;
    x2=a0+(a1*vpa(sqrt(3/5)));
    fx0=a1*vpa(f(x0));
    fx1=a1*vpa(f(x1));
    fx2=a1*vpa(f(x2));
    I=(vpa(5/9)*fx0)+(vpa(8/9)*fx1)+(vpa(5/9)*fx2);
end

if point==4
    digits(100)
    x0=a0+(a1*-vpa((sqrt(3/7-2/7*sqrt(6/5))),100));
    x1=a0+(a1*-vpa((sqrt(3/7+2/7*sqrt(6/5))),100));
    x2=a0+(a1*vpa((sqrt(3/7+2/7*sqrt(6/5))),100));
    x3=a0+(a1*vpa((sqrt(3/7-2/7*sqrt(6/5))),100));
    fx0=a1*f(x0);
    fx1=a1*f(x1);
    fx2=a1*f(x2);
    fx3=a1*f(x3);
    I=((18+vpa(sqrt(30),100))/36*fx0)+((18-vpa(sqrt(30),100))/36*fx1)+((18-vpa(sqrt(30),100))/36*fx2)+((18+vpa(sqrt(30),100))/36*fx3);
end

%Interpretation of results
