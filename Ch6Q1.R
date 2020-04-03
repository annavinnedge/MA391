install.packages("pracma")
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

##Question 1a)

lambda=3
w=0.25
f=function(x){c((-w*lambda*0.05*x[2]-lambda*(0.005)*x[1]*x[2])*((x[1]>0)*(x[2]>0)),
                 (-w*0.05*x[1]-0.005*x[2])*((x[1]>0)*(x[2]>0)))}
pts=path(f,c(5,2),deltat=1)
plot(pts[,1],pts[,2],xlim=c(0,5),ylim=c(0,2))

'**ANS**
With lamda=3 and w=0.25, starting with 5 red divisions and 2 blue divisions, Red wins in 39 hours with 2.2
remaining divisions.
'

##Question 1b)

dds=function(w, lambda){
  f=function(x){c((-w*lambda*0.05*x[2]-lambda*(0.005)*x[1]*x[2])*((x[1]>0)*(x[2]>0)),
                  (-w*0.05*x[1]-0.005*x[2])*((x[1]>0)*(x[2]>0)))}
  x=c(5,2)
  pts=path(f,x,deltat=1)
  N=length(pts[,1])
  if (pts[N,1]>pts[N,2]){
    winner="R"
    hours=min(which(pts[,2]<0))-1
    forces=as.numeric(round(pts[N,1],1))
  }
  else{
    winner="B"
    hours=min(which(pts[,1]<0))-1
    forces=as.numeric(round(pts[N,2],1))
  }
  return(list(winner=winner,hours=hours,forces=forces))
}
w=c(0.1,0.2,0.25,0.5,0.75,0.9)
ans.winner=0
ans.hours=0
ans.forces=0
l=length(w)
i=6
while (i >=0){
  res=dds(w[i],3)
  ans.winner[i]=res$winner
  ans.hours[i]=res$hours
  ans.forces[i]=res$forces
  i=i-1
}
result=data.frame(weather=w,winner=ans.winner,hours=ans.hours,forces=ans.forces)
print(result)



##question 1c)
"**ANS**
Red benefits from good weather while Blue benefits from bad weather. Because of this, it would be expected that red
attacks during good weather"


##question 1d)
w=c(0.1,0.2,0.5,0.75,0.9)
lam=c(1.5,2.0,3.0,4.0,5.0)
ans.winner=0
ans.hours=0
ans.forces=0
ans.cond1=0;ans.cond2=0
N=length(w)
for (i in N){
  for(j in l:length(lam)){
    res=dds(w[i],lam[j])
    idx=N*(i-1)+j
    ans.cond1[idx]=w[i]
    ans.cond2[idx]=lam[j]
    ans.winner[idx]=res$winner
    ans.hours[idx]=res$hours
    ans.forces[idx]=res$forces
  } 
}

result=data.frame(weather=ans.cond1,eff=ans.cond2,winner=ans.winner, hours=ans.hours, forces=ans.forces)
print(result[which(result$eff==5),])
print (result)

"**ANS**
The conclusion made in part c holds true as lambda changes. Even when firepower and weapon superiority is equal between Red and Blue,
weather is still the deciding factor in which Red wins when there is good weather and Blue wins if there is bad weather."

