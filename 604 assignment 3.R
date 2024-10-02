#Lauren Palladino POL 604 assignment 3

source("http://www.michaelperess.com/r0.r")
civwar <- read_dta("Downloads/civwar.dta")
attach(civwar)

#Q4a
nlls.probit<-function(beta,y,X){
  y<-as.matrix(y)
  X<-as.matrix(X)
  return (-(y==1) * log(pnorm(X%*%beta)) - (y==0) *
            log((1-pnorm(X%*%beta))))
}

nll.probit<-function(beta,y,X){
  return(sum(nlls.probit(beta,y,X))/length(y))
}

f1 <- war ~ lpop + polity2 + gdpen + western + eeurop + lamerica + ssafrica + asia + 
  colbrit + colfra + mtnest + muslim

X<- model.X(f1)
Y<-as.matrix(formula.data(f1)$y[[1]])

probit<-optim(c(lm(Y~X-1)$coef), nll.probit, y=Y, X=X,
              method="BFGS", hessian=TRUE)
probit

#FOC
numDeriv::grad(nll.probit,probit$par, X=X, y=Y)
#SOC
eigen(probit$hessian)

#hessian
C<-probit$hessian
V1<-solve(C)

#jacobian
jac<-numDeriv::jacobian(nlls.probit, probit$par, y=Y, X=X)
B<-t(jac)%*%jac/nrow(X)
V2<-solve(B)

#sandwich
V3<-solve(C)%*%B%*%solve(C)

easy_ses<-sqrt(diag(V1/nrow(X)))
hard_ses<-sqrt(diag(V2/nrow(X)))
robust_ses<-sqrt(diag(V3/nrow(X)))

#compare from scratch to glm command

#comparing coefs + ses from glm to scratch
probit.glm <- probit0(f1,se="classic")
probit.glm$coef
probit$par

probit.glm$se
easy_ses

#sandwich estimator ses
probit.robust<-probit0(f1, se="robust")
probit.robust$se
robust_ses

#Q4b
probit.cp.customized<-function(beta,X)
{
  k<- length(beta)
  p0<-pnorm(t(X) %*% beta)
  ret<- rep(0, k-1)
  for(i in 2:k)
  { 
    X1<-X
    X1[i]<- X1[i] +1
    p1<-pnorm(t(X1) %*% beta)
    ret[i-1]<-p1 - p0
    }
  ret
}

X_star<-colMeans(model.X(f1))
X_star[5:11]<-c(0,0,0,0,0,0,0)

d2 <- delta.method(probit.cp.customized,probit.glm$coef,probit.glm$V,X=X_star)
d2$theta <- d2$est
d2$ivs <- probit.glm$ivs[2:length(probit.glm$ivs)]
print.model(d2)

#Q4c mcfadden
probit.glm$mcfadden.r2

#Q4d mcfadden from scratch
probit.intercept.only<-optim(1, nll.probit, y=Y, X=rep(1, length(Y)), method="BFGS", hessian=TRUE)
1-probit$value/probit.intercept.only$value