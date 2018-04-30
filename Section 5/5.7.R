
rm(list = ls())
#Exercise 5.7

library(gtools)
library(MASS)

# we assume sigma is known and similar for all clusters

# data
# generate data from five multivariate normal

X = matrix(0, nrow = 1000, ncol = 2)
for (j in 1:1000){
  X[j,] <- 0.129*mvrnorm(1,c(-0.8,3.4), diag(2))+0.084*mvrnorm(1, c(1.26, -1.05), diag(2))+
    0.16*mvrnorm(1, c(6.32, 1.25), diag(2))+0.45*mvrnorm(1, c(-2.876, -0.45), diag(2))+
    0.177*mvrnorm(1, c(0,0.54), diag(2))
}


Gibbssampler<- function(X, num_samples, K){
  
n <- nrow(X)
d <- ncol(X)

Z_mat <- matrix(0, nrow= num_samples, ncol=n)
Mu_mat    <- array(dim = c(num_samples, K, d))
Pi_mat    <- matrix(0, nrow= num_samples, ncol=K)

#initials

sig_0 <- diag(2)

Mu <- matrix(0, K, d)
for (k in 1:K) {
  Mu[k,] <- rmvnorm(1,  colMeans(X), sig_0)   
  
} 


alpha_0 <- 1
alpha <- rep(alpha_0, K)
Pi <- rdirichlet(1, alpha)

 
  for (i in 1:num_samples) {
    
    
    z <- matrix(0, n)

    for (t in 1:n) {
      weights <- apply(t(1:K), 2, function(k) Pi[k] * dmvnorm(X[t, ], Mu[k, ], sig_0))
      z[t] <- which.max(rmultinom(1, 1, weights))       
    }
    Z_mat[i, ] <- z


#posterior on pi, mu

alpha_n <- matrix(0, ncol=K)

for (k in 1:K){
  alpha_n[1,k] = alpha_0 + sum(Z_mat[i,]==k)
}

Pi<- rdirichlet(1, alpha_n)
Pi_mat[i,] <- Pi

for(k in 1:K){
  n_k <- sum(Z_mat[i,]==k)
  if (n_k > 1) {
  X_k <- X[Z_mat[i,]==k,]
  A<- solve(cov(X_k))
  sig_n <- solve(solve(sig_0)+n_k*A)
  mu_n <- sig_n%*%((n_k*(A%*%colMeans(X_k)))+solve(sig_0)%*%colMeans(X))
  Mu[k,]<- mvrnorm(1, mu_n, sig_n)
  }
  
  else {
    
    Mu[k,]<- mvrnorm(1, colMeans(X), sig_0)
  }

  Mu_mat[i,k,]<- Mu[k,]
}



}
  
 return(list("Pi" = Pi_mat, "Mu" = Mu_mat, "Z" = Z_mat) )
}

Gibbssampler(X, 1000, 5)