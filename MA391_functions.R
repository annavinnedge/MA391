fprime = function (f,a,h=0.0001){(f(a+h)-f(a-h))/(2*h)}


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



bisection = function(f,a,b,tol=0.0001){
  if (f(a)*f(b) > 0){
    return ("Boundary Conditions Not Met")
  }
  else{
    middle = a
    while (abs(f(middle))>tol){
      middle = (a+b)/2
      if (f(middle)*f(a)>0) (a = middle)
      else (b = middle)
      x=middle
      y=f(middle)
      ## if you want to "see" what happens at every step, take off the # of the next line ##
      #cat(sprintf("x-Val: %.4f ; f(x-val): %.4f\n",x,y))
    }
    return (middle)
  }
}

newton=function(f,x0,tol=.0001){
  xn=x0
  while (abs(f(xn))>tol){
    xn=xn-f(xn)/fprime(f,xn)
  }
  return (xn)
    
}



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

P = function(x){
  return(((339-0.01*x[1]-0.003*x[2])*x[1]
          +(399-0.004*x[1]-0.01*x[2])*x[2]
          -(400000+195*x[1]+225*x[2]))*-1)
}
X = list(x=seq(3000,6000,100),y=seq(6000,8000,100))
Z = Outer(P,X)
contour(x=X$x,y=X$y,z=-Z)
persp(x=X$x,y=X$y,Z,theta=40,ticktype = "detailed",shade=0.01)
  
  