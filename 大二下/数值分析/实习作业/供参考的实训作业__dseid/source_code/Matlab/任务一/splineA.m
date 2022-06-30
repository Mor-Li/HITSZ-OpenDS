function y = splineA(xd,yd,x,Ends,Ders)
n=length(xd);

h=diff(xd); h1=h(1); hn=h(n-1);
h=[h1 h1 h1 h hn hn hn];
Xd=[xd(1)-3*h1 xd(1)-2*h1 xd(1)-h1 xd xd(n)+hn xd(n)+2*hn xd(n)+3*hn];
[B,G]=coeffs(h);
if Ends(1)==2
    alpha0=G(4,1)/G(2,1);
    beta0=G(6,1)/G(2,1);
    gamma0=Ders(1)/G(2,1);
elseif  Ends(1)==1
    alpha0=G(3,1)/G(1,1);
    beta0=G(5,1)/G(1,1);
    gamma0=Ders(1)/G(1,1);
end
B0=Bi(xd(1),0,B,Xd);
Aa(1)=2/3-B0*alpha0;
Ac(1)=Bi(xd(1),2,B,Xd)-B0*beta0;
Ab(1)=0;
yd(1)=yd(1)-B0*gamma0;
for ii=2:n-1
    Aa(ii)=2/3;
    Ac(ii)=Bi(xd(ii),ii+1,B,Xd);
    Ab(ii)=Bi(xd(ii),ii-1,B,Xd);
end
if Ends(2)==2
    alphan=G(2,2)/G(6,2);
    betan=G(4,2)/G(6,2);
    gamman=Ders(1)/G(6,2);
elseif  Ends(2)==1
    alphan=G(1,2)/G(5,2);
    betan=G(3,2)/G(5,2);
    gamman=Ders(2)/G(5,2);
end
Bn1=Bi(xd(n),n+1,B,Xd);
Aa(n)=2/3-Bn1*betan;
Ac(n)=0;
Ab(n)=Bi(xd(n),n-1,B,Xd)-Bn1*alphan;
yd(n)=yd(n)-gamman*Bn1;
w=tri(Aa,Ab,Ac,yd);
a0=-alpha0*w(1)-beta0*w(2)+gamma0;
anp1=-alphan*w(n-1)-betan*w(n)+gamman;
w=[a0 w anp1];
nx=length(x);
y=zeros(1,nx);
for ix=1:nx
    sum=0;
    for k=1:n+2
        sum=sum+w(k)*Bi(x(ix),k-1,B,Xd);
    end
    y(ix)=sum;
end

% local functions used by algorithm
function y = tri( a, b, c, f )
N = length(f);
v = zeros(1,N);
y = v;
w = a(1);
y(1) = f(1)/w;
for i=2:N
    v(i-1) = c(i-1)/w;
    w = a(i) - b(i)*v(i-1);
    y(i) = ( f(i) - b(i)*y(i-1) )/w;
end
for j=N-1:-1:1
    y(j) = y(j) - v(j)*y(j+1);
end

function [B,G] =coeffs(h)
N=length(h);
B=zeros(N-3,16);
G=zeros(6,2);
for j=1:N-3
    hm2=h(j); hm1=h(j+1); h1=h(j+2); h2=h(j+3);
    d2=solver(hm2,hm1,h1,h2);
    B(j,4)=d2(1);
    B(j,5)=d2(1)*hm2^3;
    B(j,6)=3*d2(1)*hm2^2;
    B(j,7)=3*d2(1)*hm2;
    Tm3=(hm2+hm1)^3-hm1^3;
    B(j,8)=(2/3-Tm3*d2(1))/hm1^3;
    B(j,9)=-d2(2)*h2^3;
    B(j,10)=3*d2(2)*h2^2;
    B(j,11)=-3*d2(2)*h2;
    T3=(h2+h1)^3-h1^3;
    B(j,12)=(-2/3-T3*d2(2))/h1^3;
    B(j,16)=d2(2);
    if j==1
        G(1,1)=3*d2(2)*h2^2;
        G(2,1)=-6*d2(2)*h2;
    end
    if j==2
        G(3,1)=3*d2(2)*h2*(h2+2*h1)+3*B(j,12)*h1^2;
        G(4,1)=-6*(d2(2)*h2+B(j,12)*h1);
    end
    if j==3
        G(5,1)=3*d2(1)*hm2^2;
        G(6,1)=6*d2(1)*hm2;
    end
    if j==N-5
        G(1,2)=3*d2(2)*h2^2;
        G(2,2)=-6*d2(2)*h2;
    end
    if j==N-4
        G(3,2)=3*d2(2)*h2*(h2+2*h1)+3*B(j,12)*h1^2;
        G(4,2)=-6*(d2(2)*h2+B(j,12)*h1);
    end
    if j==N-3
        G(5,2)=3*d2(1)*hm2^2;
        G(6,2)=6*d2(1)*hm2;
    end
    
end

function d2=solver(hm2,hm1,h1,h2)
a=h1*hm2*(hm2+hm1)^2;
b=-hm1*h2*(h2+h1)^2;
c=h1^2*hm2*(hm2+hm1)*(hm2+2*hm1);
d=hm1^2*h2*(h2+h1)*(h2+2*h1);
A=[[a b];[c d]];
bb=[2*(hm1+h1)/3; -2*(hm1^2-h1^2)/3];
d2=A\bb;

function g=Bi(x,i,B,Xd)
j=i+1;
if x<Xd(j) || x>Xd(j+4)
    g=0;
elseif x<Xd(j+1)
    g=B(j,4)*(x-Xd(j))^3;
elseif x<Xd(j+2)
    g=B(j,5)+B(j,6)*(x-Xd(j+1))+B(j,7)*(x-Xd(j+1))^2+B(j,8)*(x-Xd(j+1))^3;
elseif x<Xd(j+3)
    g=B(j,9)+B(j,10)*(x-Xd(j+3))+B(j,11)*(x-Xd(j+3))^2+B(j,12)*(x-Xd(j+3))^3;
else
    g=B(j,16)*(x-Xd(j+4))^3;
end



















