library(MASS);library(NlcOptim)
file.choose()
### Number 1a ####
f = function(x){x[1]^2+x[2]^2}
Aeq = matrix(c(1,2),nrow=1)
Beq = matrix(5)
x0=c(5,0)
ans = solnl(x0,f,Aeq=Aeq,Beq=Beq)
print(ans)

### Number 1b
f = function(x){(x[1]^2-x[2]^2)*-1}
con = function(x){
  f = NULL
  f=rbind(f,x[1]^2-2*x[2])
  return(list(ceq=f,c=NULL))
}
x0=c(1,1)
ans = solnl(x0,f,confun = con)
print(ans)
X = list(x=seq(-5,5,0.1),y=seq(-5,5,0.1))
Z = Outer(f,X)
contour(x=X$x,y=X$y,z=-Z,lwd=3)
points(X$x,X$x^2/2,type="l",col="red")

### Number 1c (Min)
f = function(x){exp(-x[1]*x[2]/4)}
con = function(x){
  f = NULL
  f=rbind(f,x[1]^2+x[2]^2-1)
  return(list(ceq=f,c=NULL))
}
x0=c(1,0)
ans = solnl(x0,f,confun = con)
print(ans)
X = list(x=seq(-1.5,1.5,0.1),y=seq(-1.5,1.5,0.1))
Z = Outer(f,X)
contour(x=X$x,y=X$y,z=Z,lwd=3)
r = function(t){c(sin(t),cos(t))}
r = Vectorize(r)
t = seq(0,2*pi,0.1)
lines(r(t)[1,],r(t)[2,],col="red")
### Number 1c (Max)
f = function(x){exp(-x[1]*x[2]/4)*-1}

con = function(x){
  f = NULL
  f=rbind(f,x[1]^2+x[2]^2-1)
  return(list(ceq=f,c=NULL))
}
x0=c(1,-1)
ans = solnl(x0,f,confun = con)
print(ans)
X = list(x=seq(-1.5,1.5,0.1),y=seq(-1.5,1.5,0.1))
Z = Outer(f,X)
contour(x=X$x,y=X$y,z=-Z,lwd=3)
r = function(t){c(sin(t),cos(t))}
r = Vectorize(r)
t = seq(0,2*pi,0.1)
lines(r(t)[1,],r(t)[2,],col="red")
### Number 1d
f = function(x){(x[1]^2+x[2]^2+x[3]^2)*-1}
x0=c(1,1,1)
Aeq = matrix(c(1,0,2,1,1,0),nrow=2,byrow=TRUE)
Beq = matrix(c(6,12))
A = matrix(c(-1,0,0,0,-1,0,0,0,-1),nrow=3,byrow=T)
B = matrix(c(0,0,0))
solnl(x0,f,Aeq=Aeq,Beq=Beq,A=A,B=B)

### Number 2
f = function(x){(-2*x[1]^2-x[2]^2+x[1]*x[2]+8*x[1]+3*x[2])*-1}
Aeq = matrix(c(3,1),nrow=1)
Beq = matrix(10)
x0=c(1,1)
solnl(x0,f,Aeq=Aeq,Beq=Beq)
### Number 3
f = function(x){((30-x[1])*x[1]+(50-2*x[2])*x[2]-10*x[3]-3*x[1]-5*x[2])*-1}
A = matrix(c(1,1,-1,0,0,1),nrow=2,byrow=T)
B = matrix(c(0,17.25))
x0=c(1,1,1)
solnl(x0,f,A=A,B=B)
### Number 4
f = function(x){3*sqrt(x[1]^2+2^2)+2*sqrt(x[2]^2+1)+1*x[3]}
Aeq = matrix(c(1,1,1),nrow=1)
Beq = matrix(10)
x0=c(3,3,4)
solnl(x0,f,Aeq=Aeq,Beq=Beq)
### Number 5

f = function(x){x[1]*x[2]+5*x[1]*x[2]+3*(2*x[1]*x[3]+2*x[2]*x[3])}
con = function(x){
  f = NULL
  f = rbind(f,x[1]*x[2]*x[3]-1000)
  return(list(ceq=f,c=NULL))
}
A = matrix(c(-1,0,0,0,-1,0,0,0,-1),nrow=3,byrow=T)
B = matrix(c(-1,-1,-1))
print(A)
print(B)
x0=c(5,3,1000/15)
ans=solnl(x0,f,confun = con,A=A,B=B)
print(ans$par)