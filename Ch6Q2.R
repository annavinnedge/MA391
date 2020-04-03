##question 2)

library(pracma)
Jacobian2 = function(f,x0,h=1E-4){
  jax = matrix(0,nrow=2,ncol=2)
  xph = c(x0[1]+h,x0[2]);xmh=c(x0[1]-h,x0[2])
  yph = c(x0[1],x0[2]+h);ymh=c(x0[1],x0[2]-h)
  jax[,1]=(f(xph)-f(xmh))/(2*h)
  jax[,2]=(f(yph)-f(ymh))/(2*h)            
  return(jax)
}
norm2 = function(v){sqrt(v[1]^2+v[2]^2)}
zeros = function(f,x0,h=1E-4,tol=1E-4){
  i = 1
  p=c(1,1)
  while (norm2(p)>tol & i<100){
    p = solve(Jacobian2(f,x0),-f(x0)) # linear algebra step
    x0 = x0+p
    #print(i)
    #print(x0)
    i=i+1
  }
  return(x0)
}
path = function(f,x0,deltat=0.01,N=1000,tol=1E-4){
  points=matrix(0,ncol=2)
  points[1,] = x0
  n = 0
  p = c(1,1)
  while(norm2(p)>tol & n<N){
    n=n+1
    p = f(x0)*deltat
    x0=x0+p
    points = rbind(points,x0)
  }
  
  rownames(points)=0:n
  return(points)
}
lambda=2
f=function(x){c((-w*lambda*0.05*x[2]-lambda*(0.005)*x[1]*x[2])*((x[1]>0)*(x[2]>0)),
                (-w*0.05*x[1]-0.005*x[2])*((x[1]>0)*(x[2]>0)))}
#no hold
x=c(5,2)
forces=path(f,x,deltat=1)
tail(forces)

#hold for 1 day
x=c(3,2)
forces=path(f,x,deltat=1,N=25)
x=tail(forces,1)+c(2,0)
forces=path(fx,deltat=1)
tail(forces)

#hold for 2 days
x=c(3,2)
forces=path(f,x,deltat=1,N=25)
x=tail(forces,1)+c(2,0)
forces=path(fx,deltat=1)
tail(forces)

relu=function(x){
  for (i in l:length(x)){
    if (x[i]<0){x[i]=0}
    
  }
  return(x)
}
t=c(1.2,-2.8,3,-10)
relu(t)

simulate = function(lambda){
  f=function(x){c((-w*lambda*0.05*x[2]-lambda*(0.005)*x[1]*x[2])*((x[1]>0)*(x[2]>0)),
                  (-w*0.05*x[1]-0.005*x[2])*((x[1]>0)*(x[2]>0)))
    
    if (res[1]+x[1]<0){res[1]=-x[1]}
    if (res[2]+x[2]<0){res[2]=-x[2]}
    return(res)
  }
  
  x=c(5,2)
  force0=path(f,x,deltat=1,N=12)
  print(force1)
  x=tail(force1,1)+c(2,0)
  force1=path(f,x,deltat=1)
  hold1=tail(force1)[1]
  
  return(list(hold0=hold0,hold1-hold1,hold2=hold2))
  
}
    
lam=c(1.0,1.5,2.0,3.0,5.0,6.0)
ans.f0=0
ans.f1=0
ans.f2=0

for (i in length(lam)){
  res=simulate(lam[i])
  ans.f0[i]=res$hold0
  ans.f1[i]=res$hold1
  ans.f2[i]=res$hold2
}

result=data.frame(lambda=lam,f0=ans.f0,f1=ans.f1,f2=ans.f2)
print(result)



"**ANSWERS**
b)If one day is waited for reinforcements, red wins after 17 hours with 2.9 divisions left
  If two days are waited for reinforcements, red wins after 26 hours with 2.6 divisions left

c)If the Red commander chooses to commit all forces on day 1, Red will win after 9 hours and
will have 3.7 divisions left. This is a better option than waiting one or two additional days which
both result in more casualties and take longer to achieve the same result

d)No matter to what extent Blue has weapon superiority (adjusting lambda),Red will almost always win
if they attack on the first day. If I was the red commander, I would definitely commit all forces 
towards a day 1 attack."
