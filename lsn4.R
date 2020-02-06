# LESSON 4 
# Problem 7 - Pig ###

# (a)
f = function(x){((200+5*x)*(0.65-0.01*x)-(100+0.45*x))/(x+90)}
dF = fprime(f,x)

ans = bisection(dF,0,40)
print(ans)

# We want to sell the pig after 34 days (from 33.8).

# (b)
ans.rate=0
ans.cost=0
r = seq(2/7,8/7,1/7)
for (i in 1:length(x)){
  ## Change r to the variable r[i] to iterate the changing rate
  fT = function(x){return(((200+5*x[i])*(0.65-0.01*x[i])-(100+0.45*x[i]))/(x[i+90)}
  dfT = function(x){fprime(fT,x)}
  ans.rate[i] = bisection(dfT,0,40)
  ans.cost1[i] = fT(ans.rate[i])
}
result = data.frame(rate = r, crew = ans.rate, cost = ans.cost1)
print(result)

# (c)
ans.rate=0
ans.cost=0
r = seq(2/7,8/7,1/7)
for (i in 1:length(x)){
  ## Change r to the variable r[i] to iterate the changing rate
  fT = function(x){return(((200+5*x[i])*(0.65-0.01*x[i])-(100+0.45*x[i]))/(x[i+90)}
  dfT = function(x){fprime(fT,x)}
  ans.rate[i] = bisection(dfT,0,40)
  ans.cost1[i] = fT(ans.rate[i])
}
result = data.frame(rate = r, crew = ans.rate, cost = ans.cost1)
print(result)

### Problem 8 - Pig ###
# (a)
p=function(t){(0.65-0.01*t)*(200+5*t-t^2/60)-.45*t}
dP=function(t){fprime(p,t)}
dP(1)
t=seq(0,20)
ans=bisection(dP,0,20)
newton(dP,10)
plot(t,p(t))

print(ans)
print(p(ans))

# At day 7 (from 6.8) the price will be $132.68.

# (b)
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

# The profit does not change enough with the difference in days so selling on day 7 will be fine for maximizing the profit.
