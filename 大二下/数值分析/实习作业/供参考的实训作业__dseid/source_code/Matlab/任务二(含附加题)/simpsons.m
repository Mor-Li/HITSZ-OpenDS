function I = simpsons(f,a,b,n)
% This function computes the integral "I" via Simpson's rule in the interval [a,b] with n+1 equally spaced points
% 
% Syntax: I = simpsons(f,a,b,n)
% 
% Where,
%  f= can be either an anonymous function (e.g. f=@(x) sin(x)) or a vector
%  containing equally spaced values of the function to be integrated
%  a= Initial point of interval
%  b= Last point of interval
%  n= # of sub-intervals (panels), must be integer
% 
%  Written by Juan Camilo Medina  - The University of Notre Dame
%  09/2010 (copyright Dr. Simpson)
% 
% 
% Example 1:
% 
% Suppose you want to integrate a function f(x) in the interval [-1,1].
% You also want 3 integration points (2 panels) evenly distributed through the
% domain (you can select more point for better accuracy).
% Thus:
%
% f=@(x) ((x-1).*x./2).*((x-1).*x./2);
% I=simpsons(f,-1,1,2)
% 
% 
% Example 2:
% 
% Suppose you want to integrate a function f(x) in the interval [-1,1].
% You know some values of the function f(x) between the given interval,
% those are fi= {1,0.518,0.230,0.078,0.014,0,0.006,0.014,0.014,0.006,0}
% Thus:
%
% fi= [1 0.518 0.230 0.078 0.014 0 0.006 0.014 0.014 0.006 0];
% I=simpsons(fi,-1,1,[])
%
% note that there is no need to provide the number of intervals (panels) "n",
% since they are implicitly specified by the number of elements in the
% vector fi

if numel(f)>1 % If the input provided is a vector
    n=numel(f)-1; h=(b-a)/n;
    I= h/3*(f(1)+2*sum(f(3:2:end-2))+4*sum(f(2:2:end))+f(end));
else % If the input provided is an anonymous function
    h=(b-a)/n; xi=a:h:b;
    I= h/3*(f(xi(1))+2*sum(f(xi(3:2:end-2)))+4*sum(f(xi(2:2:end)))+f(xi(end)));
end
