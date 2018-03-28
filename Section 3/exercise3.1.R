
##Exercise 3.1

## required library
#install.packages("mvtnorm")
#install.packages("truncnorm")
library(mvtnorm)
library(truncnorm)

##gibbs sampler

sampler<-function(Y,X,num_samples=1000,burnin=100) {
  
  z <-matrix(0, nrow(Y),num_samples+burnin)
  beta<-matrix(0, num_samples+burnin, ncol(X))
  
  
  for (i in 1:(num_samples+burnin)) {
   
    
    ##posterior Parameters of Beta
    
    mu_beta <- ((solve((t(X)%*%X)+diag(ncol(X))))%*%t(X))%*% z[,i]
    Var_beta <- solve((t(X)%*%X)+diag(ncol(X)))  
    beta[i,] <- rmvnorm(n=1,mean=mu_beta,sigma=Var_beta)
    mu_z <- X%*%(beta[i,])
    tau <-1
    
    # Draw latent variable z from its full conditional: z | beta, y, X
    z[Y==1] <- rtruncnorm(n=1, mean = mu_z, sd = tau, a = -Inf, b = 0)
    z[Y==0] <- rtruncnorm(n=1, mean = mu_z, sd = tau, a = 0, b = Inf)
    
  }
  
  return(list("beta"= beta))
}

## importing data
data = read.csv("C:/Users/Sareh/Google Drive/PhD/Term 7/Courses/Statistical Modeling 2/Section 3/pima.csv", header = TRUE)

X <- as.matrix(data[,1:8])

Y <- as.matrix(data$class_variable)

result = sampler(Y,X)
beta = result$beta

plot(beta[,1],type="l")

