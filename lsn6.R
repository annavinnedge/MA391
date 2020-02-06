#Lsn 6
#ch2, q. 5,7

Outer = function(f,x){
  n1 = length(x[[1]])
  n2 = length(x[[2]])
  res = matrix(0,nrow=n1,ncol=n2)
  rownames(res) = x[[1]]
  colnames(res) = x[[2]]
  for (i in 1:n1){
    for (j in 1:n2){
      res[i,j]=f(c(x[[1]][[i]],x[[2]][[j]]))
    }
  }
  return(res)
}

#question 5-TV problem

#part a)
P = function(x){
  return((((339-0.01*x[1]-0.003*x[2])*x[1]
          +(399-0.004*x[1]-0.01*x[2])*x[2]
          -(400000+195*x[1]+225*x[2]))-25*(x[1]+x[2]))*-1)
}


x0=c(10000,10000)
P(x0)
optim(x0,P)

#optimal TV numbers: 19 inch-3809, 21 inch-6117, profit=$282345

#part b)
P1 = function(a){
  P = function(x){
    return((((339-0.01*x[1]-0.003*x[2])*x[1]
           +(399-0.004*x[1]-0.01*x[2])*x[2]
           -(400000+195*x[1]+225*x[2]))-350000)*-1)
  }
}


x0=c(10000,10000)
P(x0)
optim(x0,P)

#total profit=203641 which is less than with the tariffs so it would not be worthwhile to relocate production

#part c

P1 = function(a){
  Pnew = function(x){
   return((((339-0.01*x[1]-0.003*x[2])*x[1]
           +(399-0.004*x[1]-0.01*x[2])*x[2]
           -(400000+195*x[1]+225*x[2]))-z*(x[1]+x[2]))*-1)
  }
  return(f(x))
}
z = seq(0,50,1)
for (i in 1:length(z)){
  ans.profit[i]=-P1(a[i])
  ans.maxProfit[i] = -P(a[i])$value
}
result = data.frame(a=a,change=(a-0.01)/0.01,profit=ans.profit,max_profit=ans.maxProfit,
                    diff=round((ans.maxProfit-ans.profit)/ans.maxProfit,4))
print(result)
##example
P1 = function(a){
  f = function(x){((339-a*x[1]-0.003*x[2])*x[1]
                   +(399-0.004*x[1]-0.01*x[2])*x[2]
                   -(400000+195*x[1]+225*x[2]))*-1
  }
  x = c(4735,7042) 
  return(f(x))
}
a = seq(0.01,0.02,0.0005)
ans.profit = 0
ans.maxProfit=0
for (i in 1:length(a)){
  ans.profit[i]=-P1(a[i])
  ans.maxProfit[i] = -P(a[i])$value
}
result = data.frame(a=a,change=(a-0.01)/0.01,profit=ans.profit,max_profit=ans.maxProfit,
                    diff=round((ans.maxProfit-ans.profit)/ans.maxProfit,4))
print(result)
##end example

#how do i get this loop to run through the function?
z = seq(0,50,1)
ans.x1=0
ans.x2=0
ans.profit=0
for(i in 1:length(z)){
  ans = P(z[i])
  ans.profit[i] = -ans$value
}
result = data.frame(z=z,profit=format(ans.profit,big.mark=","))
print(result)


x0=c(10000,10000)
P(x0)
optim(x0,P)



#visual representations
X = list(x=seq(3000,6000,100),y=seq(6000,8000,100))
Z = Outer(P,X)
contour(x=X$x,y=X$y,z=-Z)
persp(x=X$x,y=X$y,Z,theta=40,ticktype = "detailed",shade=0.01)


Product = function(f,X){
  #X is a list with two vector of numbers, f is a function that returns the optimal f
  #output is a table of x,y,f(x,y) values and a matrix of x,y,f(x,y) values
  col1=0
  col2=0
  l = length(X[[1]])*length(X[[2]])
  fun=data.frame(x=1:l,y=1:l,f=1:l)
  n = 0
  z = matrix(0,nrow=length(X[[1]]),ncol=length(X[[2]]))
  for (i in 1:length(X[[1]])){
    for (j in 1:length(X[[2]])){
      n=n+1
      fVal = f(c(X[[1]][i],X[[2]][j]))
      fun$x[n]=X[[1]][i]
      fun$y[n]=X[[2]][j]
      fun$f[n]=fVal
      z[i,j]=fVal #matrix of z values for all (x row, y col)
    }
  }
  #df is a table of x*y rows of function values, fun is a matrix of x rows and y cols
  ans = list(df=fun,z=z)
  return(ans)
}
Z=Product(P,X)

library(ggplot2)
library(RColorBrewer)
df = Z$df
v = ggplot(df, aes(x, y, z = f))
v=v + geom_contour(aes(colour = stat(level)))
v


v=ggplot(df, aes(x, y, z = f))+
  geom_tile(aes(fill=f))+
  stat_contour(bins=6,aes(x,y,z=f), color="black", size=0.6)+
  scale_fill_gradientn(colours=brewer.pal(6,"YlOrRd"))
v


## gradient function ##
grad = function(f,x,h=0.01){
  n = length(x)
  delF = array(0,dim=c(n,1))
  for (i in 1:n){
    xhp = xhm = x
    
    xhp[i]=xhp[i]+h
    xhm[i]=xhm[i]-h
    delF[i] = (f(xhp)-f(xhm))/(2*h)
  }
  return (delF)
}

## Hessian Function ##
hessian = function(f,x,hs=0.01){
  n = length(x)
  H = array(0,dim=c(n,n))
  
  for (i in 1:n){
    h = array(0.0,dim=c(n,1))
    h[i] = hs
    H[i,]=t((grad(f,x+h)-grad(f,x-h))/(2*hs))
    #print(H[i,])
  }
  return (H)
}



#----------------------------------------------------------------------------------------------------------------------------
#question 7 
#Unconstrained Optimization Problem
  #A  local  daily  newspaper  has  recently  been  acquired  by  a  large  media  conglomerate.   The  paper
  #currently  sells  for  $1.50/week  and  has  a  circulation  of  80,000  subscribers.   Advertising  sells  for
  #$250/page, and the paper currently sells 350 pages/week (50 pages/day).  The new management
  #is looking for ways to increase profits.  It is estimated that an increase of ten cents/week in the
  #subscription  price  will  cause  a  drop  in  circulation  of  5,000  subscribers.   Increasing  the  price  of
  #advertising by $100/page will cause the paper to lose approximately 50 pages of advertising per
  #week.  The loss of advertising will also a ect circulation, since one of the reasons people buy the
  #paper is for the advertisements.  If is estimated that a loss of 50 pages of advertisements per week
  #will reduce circulation by 1,000 subscriptions.
  
#a).  Find the weekly subscription price and the advertising price that will maximize profit.

# variable
# p = price of paper subscription ($/week)
# q = price of advertising ($/page)
# s = number of subscribers (per week)
# d = number of pages of advertisings (per week)
# c = number of $0.10 raises in subscription price
# a = number of $100 raises in advertising price
# P = profit($/week)

# Assumption
# s = 80000 - 5000c - 1000a
# d = 350-50a
# p = 1.5 + 0.1c
# q = 250 + 100a
# P = ps +qd

# objective: maximize profit

f=function(x){(((1.5+.1*x[1]*(80000-5000*x[1]-1000*x[2])+(250+100*x[2]*(350-50*x[2]))))*-1)}

X = list(x=seq(3000,6000,100),y=seq(6000,8000,100))
Z = Outer(f,X)
contour(x=X$x,y=X$y,z=-Z)
persp(x=X$x,y=X$y,Z,theta=40,ticktype = "detailed",shade=0.01)

x0=c(50,50)
f(x0)
optim(x0,f)



library(MASS);library(NlcOptim)
source(file.choose())

#problem 3
##Transform: 
#facts/variables
#x1=blue whale pop(whales)
#x2=fin whale pop(whales)
#B=blue whale harvest rate (whales/yr)
#F=fin whale harvest rate (whales/yr)
#R=revenue
#x1>0,x2>0
#blue whale=$12000
#fin whale=$6000

#Assumptions:
#Intrinsic growth rate is constant
#growth rate is based on current population
#growth rate is subject to logistic growth with a carrying capacity
#we want growth rate = harvest rate (equilibrium point)
#B=0.05 x1 (1 - x1 / 150000) - 10^(-8) x1*x2
#F=0.08 x2 (1 - x2 / 400000) - 10^(-8) x1 *x2
#revenue is only based on harvests of blue and fin whales
#R=12(B)+6(F)
f = function(x){(x[1]*x[2]-2*x[1]-2*x[2]-x[1]^2-x[2]^2)*(-1)}
x=c(0,0)
optim(x,f)
Blue = function(x){(0.05*x[1]*(1-x[1]/150000)-1/100000000*x[1]*x[2])}
Fin = function(x){(0.08*x[2]*(1-x[2]/400000)-1/100000000*x[1]*x[2])}
Rev = function(x){(12*(0.05*x[1]*(1-x[1]/150000)-1/100000000*x[1]*x[2]) +
                     6*(0.08*x[2]*(1-x[2]/
                                     400000)-1/100000000*x[1]*x[2]))*(-1)}
x = c(50000,50000)
ans=optim(x,Rev,method="L-BFGS-B")
print(ans$par)
print(-ans$val)
Blue(ans$par)
Fin(ans$par)
R2 = function(r2){
  fr2 = function(x){(12*(0.05*x[1]*(1-x[1]/150000)-1/100000000*x[1]*x[2]) +
                       6*(r2*x[2]*(1-x[2]/400000)-1/100000000*x[1]*x[2]))*(-1)}
  x = c(50000,50000)
  ans=optim(x,fr2,method="L-BFGS-B")
  return (ans)
}
R2(0.08)
ans.x1=0
ans.x2=0
ans.rev=0
r = seq(.04,.12,.01)
for (i in 1:length(r)){
  ans=R2(r[i])
  ans.x1[i]=ans$par[1]
  ans.x2[i]=ans$par[2]
  ans.rev[i]=-ans$value
}
result = data.frame(growth_rate=r,x1=ans.x1,x2=ans.x2,rev=ans.rev)
print(result)

