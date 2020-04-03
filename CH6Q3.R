##Question 3a)

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

x=c(5*.3,2*.65)
lambda=3
f=function(x){
  res=c((-w*lambda*0.05*x[2]-lambda*(0.005)*x[1]*x[2])*((x[1]>0)*(x[2]>0)),
                         (-w*0.05*x[1]-0.005*x[2])*((x[1]>0)*(x[2]>0)))
    
  if (res[1]+x[1]<0){res[1]=-x[1]}
  if (res[2]+x[2]<0){res[2]=-x[2]}
  return(res)
}

battle=path(f,x,deltat=1)
print(battle)

"**ANS**
a) Assuming the Blue commander calls for an immediate nuclear strike, Blue wins the battle in 9 hours and has
0.9 divisions left. Blue benefits from calling a nuclear attack because it allows them to win the battle. Under
normal batte circumstances, Red would win in the first day "

##Question 3b)
x=c(5,2)
sixHours=path(f,x,deltat=1,N=6)
print(sixHours)
x=tail(sixHours,1)
x[1]=.3*x[1];x[2]=.65*x[2]
battle2=path(f,x,deltat=1)
print(battle2)

"**ANS**
b)If Blue commander waits for six hours to call the nuclear strike, Red will win in 16 hours with 0.7 divisions remaining.
This is better for Blue than the regular scenario because they survive longer and there are more Red casualties,
but iit is not as good as the immediate nuclear strike which would allow Blue to win"

##Question 3c)
"**ANS**
A tactical nuclear strike by Blue is the difference between winning or loosing the battle. If Blue strikes immediately,
they would be able to win whereas under conventional tactics, Red would win."

##Question 3d)
nukesim=function(lambda,N,nuke){
  f=function(x){
    res=c((-w*lambda*0.05*x[2]-lambda*(0.005)*x[1]*x[2])*((x[1]>0)*(x[2]>0)),
          (-w*0.05*x[1]-0.005*x[2])*((x[1]>0)*(x[2]>0)))
    
    if (res[1]+x[1]<0){res[1]=-x[1]}
    if (res[2]+x[2]<0){res[2]=-x[2]}
    return(res)
    
  }
  x=c(5,2)
  b1=path(f,x,deltat=1,N=N)
  if (nuke){x=tail(b1,1)x[1]=0.3*x[1];x[2]=0.65*x[2]}
  b2=path(f,x,deltat=1)
  forces=tail(b2,1)
  if (forces[1]>forces[2]{winner="Red"}else{winner="Blue"})
    forces=round(forces,1)
  return (list(R=forces[1],B=forces[2],winner=winner))
  
}

N=seq(0,6,1)
lam=c(1.0,1.5,2.0,3.0,4.0,5.0,6.0)
ans.adv=0
ans.hour=0
ans.win=0
ans.red=0
ans.blue=0
for(i in l:length(lan)){
  for (j in l:length(N)){
    res=nukesim(lam[i],N[j],T)
    idx=(i-1)*length(N)+j
    ans.adv[idx]=lam[i]
    ans.hour[idx]=N[j]
    ans.win[idx]=res$winner
    ans.red[idx]=res$forcesans.blue[idx]=res$B
    
    
  }
}

for (i in l:length(lam)){
  idx=idx+1
  res=nukesim(lam[i],0,F)
  ans.adv[idx]=lam[i]
  ans.hour[idx]=NA
  ans.win[idx]=res$winner
  ans.red[idx]=res$forcesans.blue[idx]=res$B
  
}
result=data.frame(adv=ans.adv,hour=ans.hour,winner=ans.win,red=ans.red,blue=ans.blue)
print(result)

