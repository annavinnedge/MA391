
source(file.choose())
fprime = function (f,a,h=0.0001){(f(a+h)-f(a-h))/(2*h)}


p=function(t){(0.65-0.01*t)*(200+5*t-t^2/60)-.45*t}
dP=function(t){fprime(p,t)}
dP(1)
t=seq(0,20)
ans=bisection(dP,0,20)
newton(dP,10)
plot(t,p(t))

print(ans)
print(p(ans))


ans.time=0
ans.profit=0
months=seq(1,10)
for (i in 1:length(months)){
  m=months[i]
  p=function(t){(0.65-0.01*t)*((5/m)*(m*t-t^2/60)+200)-.45*t}
  dP=function(t){fprime(p,t)}
  ans.time[i]=bisection(dP,0,20)
  ans.profit[i]=p(ans.time[i])
  
             
}
result=data.frame(months,time=ans.time,profit=ans.profit)
print(result)
