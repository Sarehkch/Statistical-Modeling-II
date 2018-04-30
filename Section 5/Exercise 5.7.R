
## libraries
library(mvtnorm)
library(MASS)
library(MCMCpack)

# data is two-dimensional

## gibbs sampler function

gibbsnorm <- function (dat, k, niter)
{
  
  
  # drichlet distribution
  
  rdirichlet <- function(n, k) {
    z <- array (0 , dim = c(n, k))
    s <- array(0, dim = c(n, 1))
    for (i in 1:k) { 
      z [, i] = rgamma(n, shape  = par [i])
      s = s + z[, i]                                         
    }
    for (i in 1:k) {
      z [ , i] = z [ , i]/s
    }
    return(z) 
  }
  
  n <- nrow(dat)
  d <-ncol(dat)
  
  ## initial values
  k_0= 1
  mu_0 <- rmvnorm(1, mean = colMeans(dat), sigma = cov(dat))
  sig_0 <- 10*diag(d)
  p <- rep(1/k , k)
  mixparam <- list(p=p, mu=mu_0, sig = sig_0)
  
  z <- rep(0,k)
  gibbsmu <- array(dim = c(niter, k,d))
  gibbssig <- array(dim = c(niter, k,d,d))
  gibbsp <- array(dim = c(niter, k))
  
  
  for (i in 1:niter) {
    for (t in 1:n){
      prob <- mixparam$p * dmvnorm(dat[t], mean = mixparam$mu, sigma = mixparam$sig)
      z[t] <- sample(x=1:k, size =1, prob)
    }
    for (j in 1:k){
      
      X <- cbind(z, dat)
      W <- X[X[,1]==j,]
      W = W[,2:3]
      nj = nrow(W)
      datbarj = colMeans(W)
      
      gibbsmu[i,j,] <- rmvnorm(k, mean = (k_0/(k_0+nj))*mu_0 + (nj/(k_0+nj))*(datbarj/nrow(datbarj)), mixparam$sig/k_0)

    mixparam$mu <- gibbsmu[i,j]

    S <- diag((W-datbar)%*%t(W-datbar))
    L<- (datbar - gibbsmu[i,j,])%*% t(datbar - gibbsmu[i,j,])
    gibbssig[i, j, , ] <- riwish(1 + d + n, sig_0 + t(S) %*% S + (k_0/(k_0+nj))*(t(L) %*% L))
    mixparam$sig <- gibbssig[i,j,]
    gibbsp[i,] <- rdirichlet(1, k )
    mixparam$p <- gibbsp[i,]
    }
  }
  data.frame(p = gibbsp, mu=gibbsmu, sigma = gibbssig)
  }


#data

data = as.matrix(read.csv("G:/My Drive/PhD/Term 7 - Spring 2018/Courses/Statistical Modeling 2/Project/data/feature results.csv", header = TRUE))
dat = data[,2:3]
result = gibbsnorm (dat, 4, 100)
