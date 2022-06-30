function final_function=least_squares(f,pointx)
clc;
syms x;

%初始化正交多项式组
p=cell(5,1);

%0次正交多项式
p{1}=1;
%1次正交多项式
p{2}= x;

%2,3,4次正交多项式

for k=2:4
    alpha(k+1) = dot(subs(p{k},x,pointx),subs(p{k}*x,x,pointx))/dot(subs(p{k},x,pointx),subs(p{k},x,pointx));
    beta(k)=dot(subs(p{k},x,pointx),subs(p{k},x,pointx))/dot(subs(p{k-1},x,pointx),subs(p{k-1},x,pointx));
    p{k+1} = (x-alpha(k+1))*p{k}-beta(k)*p{k-1};
end

%得到最终的正交多项式
final_function=0;
for k=1:5
    a(k)=dot(subs(f,x,pointx),subs(p{k},x,pointx))/dot(subs(p{k},x,pointx),subs(p{k},x,pointx));
    final_function=final_function+a(k)*p{k};
end


% end