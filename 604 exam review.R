#Lauren Palladino
# POL 604 midterm review

#Q3 numerical optimization
q3func<-function(par){
  -(6/5-exp((-1/4)*(par[1]-1)^2))*(-1-(par[2]-2*par[1]-4)^2)
}
q3optim<-optim(c(10,10), fn=q3func, method="BFGS", hessian=TRUE)
q3optim

#hessian allows us to check for global max (so BFGS is more appropriate)

#3c is local? use FOC (gradient)
library(numDeriv)
grad(q3func, q3optim$par)
#FOC is close to 0

#3d is global? use SOC (hessian)
eigen(q3optim$hessian)
#eigenvalues in the hessian are positive so pd = global max

#Q4 logit model
library(haven)
q4data <- read_dta("Downloads/mexico.dta")
attach(q4data)

f_q4i<-gift_pri~approvemucho+approvealgo+disapprovealgo+disapprovemucho+
  urban+female+educ+employed
q4i<-logit0(f_q4i)
print.model(q4i)
# interpret
  #coefs- directionally
  #std errors- are your coefs significant? 

#q4.ii
pri_92_sq<-pri_97^2
f_q4ii<-gift_pri~approvemucho+approvealgo+disapprovealgo+disapprovemucho+
  urban+female+educ+employed+pri_97+pri_92_sq
q4ii<-logit0(f_q4ii)
print.model(q4ii)

.083/(0.001*2)
summary(pri_97)

#can use wald test to compare two coefficients
wald.test(function(x) x[2]-x[3], q4ii$theta, q4ii$V)

#q3.c
X<-model.X(f_q4ii)
pri_97_range<-seq(min(pri_97), max(pri_97), 0.01)
p<-rep(NA,length(pri_97_range))
for(i in 1:length(pri_97_range)){
  temp_X<-X
  temp_X[,10]<-pri_97_range[i]
  temp_X[,11]<-pri_97_range[i]^2
  temp<-exp(temp_X%*%q4ii$theta)
  p[i]<-sum(temp/(1+temp))/nrow(X)
}
ggplot(data.frame(pri_97=pri_97_range,prob=p),
       aes(x=pri_97, y=prob))+geom_line()
#clear way to test the hypothesis

#linear pr:
  #disadvant=probability can go below 0 and above 1
  #advantage= interpretation is more straigntforward
#logit
  #disadvant=difficult to get substantive effects
  #advantage= pr alwaus makes sense
#probit
  #disadvant= even less interpretable (can use odds ratio in logit)
  #advantage= similar to logit