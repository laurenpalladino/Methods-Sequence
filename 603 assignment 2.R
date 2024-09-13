#Lauren Palladino POL 603 assignment 2
#making the sample
n <- 1000
k <- 3

x <- matrix(rnorm(n*k), n)
dim(x)

colMeans(x) #means are close to 0
var(x) #var is close to identity matrix

#making mu and omega matrices
mu <- matrix(c(1, -1, 0.5), nrow=3, byrow=TRUE)
omega <- matrix (c(1, 0.5, 0.1, 0.5, 2, 0.3, 0.1, 0.3, 0.5), nrow=3, byrow=TRUE)

x2 <- as.matrix(rep(1, n))%*%t(as.matrix(mu)) + x%*%chol(omega)
dim(x2)
colMeans(x2)
var(x2)

#omega is positive definite
eigen(omega)

k3 <- k+1
x3 <- matrix(rep(0, n*k3), n)
dim(x3)
x3[1:n, 1] <- 1
x3[1:n, 2:k3] <- x2[1:n, 1:k]
head(x3)

#data generating process
beta0 <- matrix(c(0.5, -2, 1, 0.7), nrow = k3)
sigma0 <- 2.8
eps <- matrix(rnorm(n, 0, sigma0))
y <- x3 %*% beta0 + eps
dim(y)

#run the model
lm1 <- lm(y~x3)
summary(lm1)
lm1 <- lm(y~ -1+ x3) #remove the NAs on x31
summary(lm1)

#verify 
betahat <- solve(t(x3) %*% x3) %*% t(x3) %*% y
betahat #looks good to me

#montecarlo loop
montecarlo <- function(n = 1000, homosked = TRUE) {
  S <- 1000
  betahatsim <- matrix(rep(0, S*k3), S)
  sigmahatsqrsim <- rep(0, S)
  tstat3sim <- rep(0, S)
  zstat3sim <- rep(0, S)
  for(s in 1:S) {
    x <- matrix(rnorm(n*k), n)
    x2 <- as.matrix(rep(1, n)) %*% t(as.matrix(mu)) + x %*% chol(omega)
    x3 <- matrix(rep(0, n*k3), n)
    x3[1:n, 1] <- 1
    x3[1:n, 2:k3] <- x2[1:n, 1:k]
    if (homosked){
      eps <- rnorm(n)
    } else {
      eps <- rep(0, n)
      for (i in 1:n) {
        eps[i] <- rnorm(1)*x3[i, 3]^2
      }
    }
    y <- x3 %*% beta0 + sigma0*eps
    betahat <- solve(t(x3) %*% x3) %*% t(x3) %*% y
    betahatsim[s, ] <- betahat
    epshat <- y - x3 %*% betahat
    sigmahatsqr <- sum(epshat^2)/(n - k3)
    sigmahatsqrsim[s] <- sigmahatsqr
    varbetahat <- sigmahatsqr*solve(t(x3) %*% x3)
    beta3se <- varbetahat[3, 3]^0.5
    tstat3sim[s] <- (betahat[3] - beta0[3])/beta3se
    robustmeat <- matrix(rep(0, 16), 4)
    for (i in 1:n) {
      robustmeat <- robustmeat + epshat[i]^2 * x3[i, ] %*%
        t(x3[i, ])
    }
    robustmeat <- robustmeat/n
    robustbread <- solve(t(x3) %*% x3/n)
    varbetahatrobust <- robustbread %*% robustmeat %*% robustbread/n
    beta3serobust <- varbetahatrobust[3, 3]^0.5
    zstat3sim[s] <- (betahat[3] - beta0[3])/beta3serobust
  }
  #making the output
  out <- list(betahatsim = betahatsim, sigmahatsqrsim = sigmahatsqrsim,
              tstat3sim = tstat3sim, zstat3sim = zstat3sim, beta0 = beta0,
              sigma0 = sigma0)
  return(out)
}

#homoskedastic n=10, OLS Unbiased / Consistent (LLN)
hom10 <- montecarlo(10)
colMeans(hom10$betahatsim)  
hom10$beta0 
mean(hom10$sigmahatsqrsim) 
hom10$sigma0^2 
# tstat 
mean(abs(hom10$tstat3sim) > qt(0.975,n-k3)) 
# zstat w/ robust standard errors (CLT) 
mean(abs(hom10$zstat3sim) > qnorm(0.975))

#homoskedastic n=100, OLS Unbiased / Consistent (LLN)
hom100 <- montecarlo(100)
colMeans(hom100$betahatsim)  
hom100$beta0 
mean(hom100$sigmahatsqrsim) 
hom100$sigma0^2 
# tstat 
mean(abs(hom100$tstat3sim) > qt(0.975,n-k3)) 
# zstat w/ robust standard errors (CLT) 
mean(abs(hom100$zstat3sim) > qnorm(0.975)) 

#homoskedastic n=1000, OLS Unbiased / Consistent (LLN)
hom1000 <- montecarlo(1000)
colMeans(hom1000$betahatsim)  
hom1000$beta0 
mean(hom1000$sigmahatsqrsim) 
hom1000$sigma0^2 
# tstat 
mean(abs(hom1000$tstat3sim) > qt(0.975,n-k3)) 
# zstat w/ robust standard errors (CLT) 
mean(abs(hom1000$zstat3sim) > qnorm(0.975)) 

#heteroskedastic n=10, OLS Unbiased / Consistent (LLN)
het10 <- montecarlo(10, homosked = FALSE) #bc set if else statement within montecarlo
colMeans(het10$betahatsim)  
het10$beta0 
mean(het10$sigmahatsqrsim) 
het10$sigma0^2 
# tstat 
mean(abs(het10$tstat3sim) > qt(0.975,n-k3)) 
# zstat w/ robust standard errors (CLT) 
mean(abs(het10$zstat3sim) > qnorm(0.975))

#heteroskedastic n=100, OLS Unbiased / Consistent (LLN)
het100 <- montecarlo(100, homosked = FALSE)
colMeans(het100$betahatsim)  
het100$beta0 
mean(het100$sigmahatsqrsim) 
het100$sigma0^2 
# tstat 
mean(abs(het100$tstat3sim) > qt(0.975,n-k3)) 
# zstat w/ robust standard errors (CLT) 
mean(abs(het100$zstat3sim) > qnorm(0.975))

#heteroskedastic n=1000, OLS Unbiased / Consistent (LLN)
het1000 <- montecarlo(1000, homosked = FALSE)
colMeans(het1000$betahatsim)  
het1000$beta0 
mean(het100$sigmahatsqrsim) 
het1000$sigma0^2 
# tstat 
mean(abs(het1000$tstat3sim) > qt(0.975,n-k3)) 
# zstat w/ robust standard errors (CLT) 
mean(abs(het1000$zstat3sim) > qnorm(0.975))