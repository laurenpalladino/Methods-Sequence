#Lauren Palladino POL 604 assignment 2

source("http://www.michaelperess.com/r0.r")
library(ggplot2)
library(numDeriv)

#1.a 
func1<-function(x)
{(0.3*x^2-0.6*x)*exp(x)+0.2}  

x<-seq(-1,3,by=0.1)
y<-func1(x)
plot(x,y,type='l')
points(x,rep(0,length(x)), type='l', lty=2, col='blue')

#1.b bisection
solve.bisection <- function(func,xlow,xhigh)
{
  # check that solution is bracketed 
  flow <- func(xlow)
  fhigh <- func(xhigh)
  if(flow == 0) {
    return(xlow)
  } else if(fhigh == 0) {
    return(xhigh)
  } else if((flow < 0 & fhigh < 0) | (flow > 0 & fhigh > 0)) {
    print("error: solution is not bracketed")
    return(list(x=NA,y=NA))
  }
  
  for(iter in 1:100) {
    print(paste("iter: ",iter))
    print(paste("xlow: ",xlow))
    print(paste("flow: ",flow))
    print(paste("xhigh: ",xhigh))
    print(paste("fhigh: ",fhigh))
    
    if(abs(xhigh - xlow) < 0.00000001) {
      if(abs(fhigh) < abs(flow)) {
        return(list(x=xhigh,f=fhigh))
      } else {
        return(list(x=xlow,f=fhigh))
      }
    }
    
    xnew = 0.5 * (xlow + xhigh)
    fnew <- func(xnew)
    
    print(paste("xnew: ",xnew))
    print(paste("fnew: ",fnew))
    
    if(fnew == 0) {
      return(xnew)
    } else if((flow < 0 & fnew > 0) | (flow > 0 & fnew < 0)) {
      xhigh <- xnew
      fhigh <- fnew
    } else {
      xlow <- xnew
      flow <- fnew
    }
  }
  
  print("error: maximum number of iterations reached")
  return(list(x=NA,f=NA))
}

solve.bisection(func1,1,3)
solve.bisection(func1,0,1)

#1.c
solve.newton <- function(func,dfunc,x0)
{
  x <- x0
  f <- func(x)
  df <- dfunc(x)
  for(iter in 1:100) {
    print(paste("iter: ",iter))
    print(paste("x: ",x))
    print(paste("f: ",f))
    
    if(abs(f) < 0.00000001) {
      return(list(x=x,f=f))
    }
    
    x <- x - f / df
    f <- func(x)
    df <- dfunc(x)
  }
  
  print("error: maximum number of iterations reached")
  return(list(x=NA,f=NA))
}

#derivative: 
dfunc1<-function(x)
  {(0.6*x-0.6)*exp(x)+(0.3*x^2-0.6*x)*exp(x)}
df_num<-function(func,x,h=0.0000001){
  (func(x+h)-func(x))/h
}
  
dfunc1(2)
df_num(func1,2)
grad(func1,2,method="Richardson")

#1.d
solve.newton(func1,dfunc1,2.5)
solve.newton(func1,dfunc1,0)
solve.newton(func1,dfunc1,5)
solve.newton(func1,dfunc1,-10)
solve.newton(func1,dfunc1,10)

#plot?
x<-seq(-1,3,by=0.1)
y<-func1(x)
plot(x,y,type='l')
x_new<-2.5
abline(func1(x_new)-x_new*dfunc1(x_new),dfunc1(x_new), col='red')
points(x,rep(0,length(x)), type='l', lty=2, col='blue')

#newton's method w numerical derivatives
solve.newton.nd <- function(func,x0)
{
  x <- x0
  f <- func(x)
  h <- 0.000001
  df <- (func(x+h) - func(x)) / h
  for(iter in 1:100) {
    print(paste("iter: ",iter))
    print(paste("x: ",x))
    print(paste("f: ",f))
    
    if(abs(f) < 0.00000001) {
      return(list(x=x,f=f))
    }
    
    x <- x - f / df
    f <- func(x)
    df <- (func(x+h) - func(x)) / h
  }
  
  print("error: maximum number of iterations reached")
  return(list(x=NA,f=NA))
}

solve.newton.nd(func1,2.5)
solve.newton.nd(func1,-1)
solve.newton.nd(func1,0)
solve.newton.nd(func1,10)
solve.newton.nd(func1,900)

#solve w secant method
solve.secant <- function(func,x0,x1)
{
  x <- x1
  f <- func(x)
  xprev <- x0
  fprev <- func(xprev)
  df <- (f - fprev) / (x - xprev)
  for(iter in 1:100) {
    print(paste("iter: ",iter))
    print(paste("x: ",x))
    print(paste("f: ",f))
    
    if(abs(f) < 0.00000001) {
      return(list(x=x,f=f))
    }
    
    xprev <- x
    fprev <- f
    x <- x - f / df
    f <- func(x)
    df <- (f - fprev) / (x - xprev)
  }
  
  print("error: maximum number of iterations reached")
  return(list(x=NA,y=NA))
}

solve.secant(func1,2,3)
solve.secant(func1,0,1)
solve.secant(func1,3,0)
solve.secant(func1,1,20)
solve.secant(func1,-20,20)
solve.secant(func1,19,20)

#2.a
sample<-rnorm(1000, -0.5, sqrt(1.7))

mean(sample)

#2.b
normal.nll<-function(par,X){
  mu<-par[1]
  sigma<-par[2]
  -(-0.5*length(X)*log(2*pi)-length(X)*log(sigma)-(2*sigma^2)^(-1)*sum((X-mu)^2))
}

#2.c
opt_BFGS <- optim(par=c(0,1),fn=normal.nll,X=sample,method="BFGS", hessian=TRUE)
opt_BFGS

#check FOC
grad(normal.nll, opt_BFGS$par,method="simple", X=sample)

#check SOC
opt_BFGS$hessian
eigen(opt_BFGS$hessian)

#2.d
opt_NM<-optim(par=c(0,1),fn=normal.nll,X=sample,method="Nelder-Mead", hessian=TRUE)
opt_NM

#FOC
grad(normal.nll, opt_NM$par,method="simple", X=sample) #close to 0=good

#SOC
opt_NM$hessian
eigen(opt_NM$hessian) #all positive, so pd

#2.e
#take derivs w respect to mu
dnormal.nll<-function(par,X){
  mu<-par[1]
  sigma<-par[2]
  ret<-rep(0,2)
  ret[1]<- -sum((X-mu)/sigma)
  ret[2]<- -(-(1/sigma)*length(X)+sum(((X-mu)^2/sigma^3)))
  ret
}

#check derivatives
grad(normal.nll, c(0,1), method="Richardson", X=sample)
dnormal.nll(c(0,1), X=sample)

opt_BFGS_analyticald <- optim(par=c(0,1), fn=normal.nll, gr=dnormal.nll, X=sample, method="BFGS", hessian=TRUE)
opt_BFGS_analyticald 

#FOC
grad(normal.nll, opt_BFGS_analyticald$par,method="simple", X=sample)

#SOC
opt_BFGS_analyticald$hessian
eigen(opt_BFGS_analyticald$hessian)

