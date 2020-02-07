#PROBLEM 2.4.6#

#Part A

obj=function(x){((10000*(1+0.5*x[1]))+200*x[2])*(950-100*x[1])-(700*(10000*(1+0.5*x[1])+200*x[2]))-(50000-10000*x[2])*-1}

con=function(x){
  f=NULL
  f=rbind(f,-50000-10000*x[1])
  return(list(ceq=f,c=NULL))
}
x0=c(1,1)
ans=solnl(x0,objfun=obj,confun=con)
print(ans$par)
print(-ans$fn)
print(ans$lambda)


#Part B

obj=function(x){((10000*(1+0.5*x[1]))+200*x[2])*(950-100*x[1])-(700*(10000*(1+0.5*x[1])+200*x[2]))-(50000-10000*x[2])*-1}


#help (Outer)

obj = function(x){(10*x[1]^(0.6)*x[2]^(0.4))*(-1)}


## Contour Plot ##
X = list(x=seq(0,20,.1),y=seq(0,20,.1))
Z = Outer(obj,X)
contour(x=X$x,y=X$y,z=-Z,lwd=2)
abline(a=10,b=-5/3,col="red",lwd=2)

contour(x=X$x,y=X$y,z=-Z,lwd=2,
           levels=c(20,37.55,60,80,100,120,140,160,180))
abline(a=10,b=-5/3,col="red",lwd=1)

library(MASS);library(NlcOptim)
x0 = c(3,5)
Aeq = matrix(c(50,30),nrow=1)
Beq = matrix(300)
ans = solnl(x0,obj,Aeq=Aeq,Beq=Beq)
print(ans)

 ### Number 1 #####
obj = function(x){(1/2*(x[2]-32)*x[1]^2)*(-1)}
con = function(x){
  f = NULL
  f = rbind(f,x[2]^2*x[1]-10000)
  return(list(ceq=f,c=NULL))
   }
x0 = c(1,100)
solnl(x0,objfun = obj,confun = con)