library(lpSolve)
f.obj = c(50,20,30,80)
f.con = matrix(c(400,200,150,500,3,2,0,0,2,2,4,4,2,4,1,5),nrow=4,byrow=T)
f.dir = c(">=",">=",">=",">=")
f.rhs = c(500,6,10,8)
ans = lp("min",f.obj,f.con,f.dir,f.rhs,compute.sens = T)
print(ans$solution)
print(ans$objval)
print(ans$duals)

library(MASS);library(NlcOptim)
obj=function(x){
  return((-16*x[2]^2+(x[3]-32)*x[1]*x[2]+1/2*(x[3]-32)*x[1]^2)*(-1))
}
x = c(5.49,1.33,42.67)
print(obj(x))

con = function(x){
  f = NULL
  f = rbind(f,x[3]^2*x[1]-10000)
  f = rbind(f,-32*x[2]+(x[3]-32)*x[1])
  return(list(ceq = f,c=NULL))
}
solnl(x,objfun = obj,confun = con)


obj = function(x){
  return((600-3*x[1]+x[2])*x[1]+(800-2*x[2]+x[1])*x[2])
}
x0 = c(1,200)
print(obj(x))

con = function(x){
  f = NULL
  f = rbind(f,(x[1]-300)^2+(x[2]-300)^2-200^2)
  return(list(ceq = f, c = NULL))
}
solnl(x,objfun = obj,confun = con)
