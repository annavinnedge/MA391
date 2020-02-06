#Lsn3 
#Ch1 q.4,5,6


#4.  An oil spill has fouled 200 miles of Pacific shoreline. 
#The oil company responsible has been given 14 days to clean up the shoreline, 
#after which a fine will be levied in the amount of $10,000/day. 
#The local clean-up crew can scrub 5 miles of beach per week at a cost of $500/day.  
#Additional crews can be brought in at a cost of $18,000 plus $800/day for each crew. 
#(a) How many additional crews should be brought in to minimize the total cost to the company?  
#Use the five-step method.  How much will the clean-up cost?

library(tidyverse)
file.choose()

r = 5/7
fT = function(x){500*(200/(r*(x+1)))+(18000+800*200/(r*(x+1)))*x+(200/(r*(x+1))>14)*(10000*(200/(r*(x+1))-14))}

#200 miles of coast to clean
#x  = additional crews to hire
#d  = time it takes to clean
#r  = clean-up rate for a crew (miles/day)
#C1  = cost of local cleanup crew (dollars) =  500d 
#C2  = cost of  x  additional crews (dollars) =  (18000+800d)x 
#Cfine  = cost of fines after 14 days
#C  = total cost of clean-up =  C1+C2+Cfine 
#Assumptions:
  
 # days to complete clean-up are not affected by anything other than the number of crews (weather, etc):  d=200r(x+1) 
#cost for fines are only levied after 14 days - becomes a step function and is implemented below:

x = seq(-5,5,0.01)
f = function(x)(x<=-2)*1+(x>-2)*(x<3)*(x)+(x>=3)*(-x) 
f = Vectorize(f)
plot(x,f(x),type="l")

## Plot, type, xlab, ylab, xlim, ylim ##
x = seq(1,10,.01)
f = function(x) x^2
y=f(x)
plot(x,y,
     type="l",
     xlab="x title",
     ylab="y title",
     main = "The Title",
     xlim = c(0,5),
     ylim = c(-5,50))

## Run
x = rnorm(100)
y = 2*x+3 + rnorm(100,sd=.4)
plot(x,y)
##
ord = order(x)
plot(x[ord],y[ord],type="l", main="xaxs='i' avoids adding 4% more white space",xlab="X", ylab="Y",xlim= c(-4,4),xaxs='i')

num = 1:25
plot(num,num,pch=num,cex=c(.5,2),col=c("red","green","blue"),
     xlim=c(-1,26),
     main="Cycling 2 symbol sizes and 3 colors")
text(num-.5,num,num,adj=1,cex=1.1,col="#707070")







### Problem 5 - Fin Whale
#### (a)
#What level of effort will maximize the sustained harvest rate?
  ##### Step 1: Ask the question
  #Variables/Facts:
  
# R = growth rate of the fin whale population (per year)
# r = intrinsic growth rate = $0.08$
# K = maximum sustainable population = $400,000$
# x = current population, $x_0$ = $70,000$
# H = number of whales harvested per year = $0.00001*E*x$
# E = level of fishing effort in boat-days

#Assumptions:
  
#growth rate is constant
#whale harvest rate only depends on $E$ and $x$
  
#####Step 2: Select the modeling approach
  #Single Variable Optimization

#####Step 3: Formulate the model
## Define the function using boolean statements such as >, <=, == 
# Given: K=400,000, x=70,000
x = 70000
# Define the function
fW = function(x){(0.08*x*(1-(x/400000)))-(0.00001*E*x)} 
# Define the derivative
dfW = function(x){fprime(fW,x)}
# Define the domain to graph on
E = seq(3920,3923,1)
# Plot the function
plot(E,fW(E),type="o")
#The level of Effort to maximize the sustained harvest rate appears to be around 3921.5.
# Print the value that I estimate to be the maximum harvest rate.
print(fW(3921.5))

#The maximum sustained harvest rate is around 157.
#####Step 4: Solve the model
# Verify by actually solving the problem using the bisection method
ans = bisection(dfW,3920.5,3922.5)
print(ans)
print(fW(3922))
#####Step 5: Answer the question
#The maximum sustained harvest rate is around 156.9.

#### (b)
# the sensitivity to the intrinsic growth rate. Consider the optimum level of effort and the resulting population level.

ans.effort=0
ans.popn=0
#ans.cost1 <- na.omit(ans.cost)
r = seq(0,.15,.01)
E=3922
for (i in 1:length(r)){
  ## Change r to the variable r[i] to iterate the changing rate  
  fW = function(x){return((r[i]*x*(1-(x/K)))-(0.00001*E*x))}
  dfW = function(x){fprime(fW,x)}
  ans.effort[i] = 156.9
  ans.popn[i] = fW(ans.effort[i])
}
#result = data.frame(rate = r, effort = ans.effort, population = ans.popn)
#print(result)
#As the intrinsic growth rate increases the effort level remains the same and the resulting population increases.

#### (c)
#Examine the sensitivity to the maximum sustainable population. Consider the optimum level of effort and the resulting population level.

ans.effort=0
ans.popn=0
#ans.cost1 <- na.omit(ans.cost)
K = seq(0,800000,100000)
E=3922
r=0.08
for (i in 1:length(K)){
  ## Change r to the variable r[i] to iterate the changing rate  
  fW = function(x){return((r*x*(1-(x/K[i])))-(0.00001*E*x))}
  dfW = function(x){fprime(fW,x)}
  ans.effort[i] = 156.9
  ans.popn[i] = fW(ans.effort[i])
}
result = data.frame(maxsust= K, effort = ans.effort, population = ans.popn)
print(result)

#As the maximum sustainable population increases from 0 to 800,000 the effort level and resulting population remain the same.



##problem 5 improved 


fprime = function (f,a,h=0.0001){(f(a+h)-f(a-h))/(2*h)}

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
#Part A
r = 0.08
K = 400000
growthrate = function(x){
  return (r*x*(1-x/K))
}
d.growthrate = function(x){fprime(growthrate,x)}
p = bisection(d.growthrate,0,1000000000)
print(p) #the population at given x
HR = growthrate(p) #Maximum harvest Rate ~~ growth rate
print(HR)

E = (HR*(1/(p*0.00001)))
print(E)


#Part B
#Sensitivity to the intrinsic growth rate  #no returning the right numbers
rate = seq(0.01,0.16,0.01)
population = 0
effort = 0
for (i in 1:length(rate)){
  K = 400000
  growthrate = function(x){
    return (rate[i]*x*(1-x/K)*-1)
  }
  d.growthrate = function(x){fprime(growthrate,x)}
  population[i] = bisection(d.growthrate,0,1000000000) #200000
  print(population[i])
  HR=growthrate(population[i])
  effort[i] = HR/(population[i]*0.0001)
}
results = data.frame(rate = rate, population.level = population, effort.level = effort)
print(results)

#Part C
#Sensitivity to the maximum sustainable population  #no returning the right numbers
K = seq(100000,300000,10000)
population = 0
effort = 0
for (i in 1:length(K)){
  r = 0.08
  growthrate = function(x){
    return (r*x*(1-x/K[i])*-1)
  }
  d.growthrate = function(x){fprime(growthrate,x)}
  population[i] = bisection(d.growthrate,0,1000000000) #200000
  print(population[i])
  HR=growthrate(population[i])
  effort[i] = HR/(population[i]*0.0001)
}
results = data.frame(population.capacity = K, population.level = population, effort.level = effort)
print(results)
### Problem 6 - Whaling
####(a)
#What level of effort will maximize profit over the long term?
###Step 1: Ask the question
#: 
 #     = population (whales)
#E   = level of effort (boat-days) 
#g = growth rate (whales per year) 
#h = harvest rate (whales per year) 
#R= revenue (dollars per year) 
#C = cost (dollars per year) 
#P = profit (dollars per year)
#Assumptions: g = 0.08 x (1 - x / 400,000) h = .00001 E x R = 6000 h C = 500 E P = R - C g = h, x >= 0,E >= 0
#Objective: Maximize P.

g=function(x){ -.0012*x^2 + 490*x-4000000}

#####Step 2: Select the modeling approach
#Single Variable Optimization

#####Step 3: Formulate the model

  