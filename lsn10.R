
library(MASS);library(NlcOptim)


obj=function(x){-1*(E^((-x[1]*x[2])/4)
#constraint function#
con = function(x){
  f = NULL
  f = rbind(f, 2*x[2]-x[1]^2)
  return(list(ceq=f,c=NULL))
}
solnl(x0, objfun = obj,confun = con)
}


f=function(x){x[1]^2+x[2]^2=x[3]^2}
x0=c(1,1,1)
Aeq=matrix(c(1,0,2,1,1,0),nrow = 2,byrow=TRUE)
Beq=matrix



## Example 2.2 - Meerschaert ##

#objective function initialization
P = function(x){
  return(((339-0.01*x[1]-0.003*x[2])*x[1]
          +(399-0.004*x[1]-0.01*x[2])*x[2]
          -(400000+195*x[1]+225*x[2]))*-1)
}
###Linear Inequality Constraints##
x0=c(1000,1000)#start point estimate
A = matrix(c(1,0,1,-1,0,0,1,1,0,-1),nrow=5) #the A matrix with linear constraints
B = matrix(c(5000,8000,10000,0,0),nrow=5)#what the A matrix is equal to
print(A)
print(B)
solnl(x0,objfun=P,A=A,B=B)#checks on the boundaries of the linear inequality

#for linear equality, use Aeq and Beq instead of A and B
#for inequality, for inequality, must be less than or equal to zero (multiply by -1 to make correct)


#sensitivity analysis
b=seq(.001,.02,.001)#sequence of what b is changing by
#what we want, initialized, the information we will collect
ans.x1=0
ans.x2=0
ans.profit=0
f=function(b){
  P = function(x){
    return(((339-b*x[1]-0.003*x[2])*x[1]
            +(399-0.004*x[1]-0.01*x[2])*x[2]
            -(400000+195*x[1]+225*x[2]))*-1)
  }
  ###Linear Inequality Constraints##
  x0=c(1000,1000)
  A = matrix(c(1,0,1,-1,0,0,1,1,0,-1),nrow=5) #defining this matrix is not intuitive - make sure we talk in class
  B = matrix(c(5000,8000,10000,0,0),nrow=5)
  print(A)
  print(B)
  ans=solnl(x0,P,A=A,B=B)
  return(ans)
  
}
for(i in 1:length(b)){
  ans=f(b[i])
  ans.x1[i]=ans$par[1]
  ans.x2[i]=ans$par[2]
  ans.profit[i]=ans$fn
  
} 
results=data.frame(b=b,x1=ans.x1,x2=ans.x2,profit=ans.profit)
print(results)




#___________________________________________________________________________________________________________
Units = function(p){c((600-3*p[1]+p[2]),(800-2*p[2]+p[1]))}
P = function(p){(p[1]*Units(p)[1]+p[2]*Units(p)[2]-
                   (200*Units(p)[1]+300*Units(p)[2]))*(-1)}
x = c(100,100)
Units(c(100,100))
ans=optim(x,P)
Units(ans$par)
