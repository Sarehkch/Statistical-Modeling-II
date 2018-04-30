
#Exercise 5.5

rm(list = ls())

Gibbssample <- function (X, num_samples) {
  
  # beta prior on pi 
  
  pi <- rbeta(1, shape1 = 0.5, shape2= 0.5)
  
  
  
  # data is N(mu, 1/precision)
 
  # gamma prior on precisions
  #precision_1 <- rgamma (1, shape = 1, scale = 1/4)
  #precision_2 <- rgamma (1, shape = 1, scale = 1/5)
  
  # normal prior on means
  #mu_1 <- rnorm(1,mean = 0, sd = sqrt(1/(2*precision_1)))
  #mu_2 <- rnorm(1,mean = 1, sd = sqrt(1/(3*precision_1)))
  
  n = length(X)
  
  # Samples
  z = matrix(0, nrow =num_samples, ncol = n)
  pi_mat <- matrix(0, num_samples)
  precision_1_mat <- matrix(0, num_samples)
  precision_2_mat <- matrix(0, num_samples)
  mu_1_mat <- matrix(0, num_samples)
  mu_2_mat <- matrix(0, num_samples)
  
  ## initial values
  mu_1_n = 1;
  precision_1_n =0.01;
  mu_2_n =0.5;
  precision_2_n = 0.02; 
  pi_n = 0.5;
  
  for (i in 1:num_samples){
    
    # probability of X given z=1
    p1 <- dnorm(X, mu_1_n, sd = sqrt(1/precision_1_n))
    
    #probability of X given z=0
    p2 <- dnorm(X, mu_2_n, sd = sqrt(1/precision_2_n))
    
    #probability of z=1 given X
    P <- pi_n*p1/(pi_n*p1 + (1-pi_n)*p2)
    
    z[i,] <- as.vector(1 - (runif(n) > P))
    
    #posterior distribution of pi
    pi_n <- rbeta(1, shape1 = 0.5 + sum(z[i,]), shape2= 0.5+n-sum(z[i,]))  
    pi_mat[i] <- pi_n
    
    # sum of data assigned to group 1
    X_1 <- sum(X[z[i,]==1])
    # number of data belongs to group 1 
    n_1 <- sum(z[i,]==1)
    # standard deviation
    S_1 <- 0.5* sum((X[z[i,]==1]-(X_1/n_1))^2)
    # data assigned to group 0
    X_2 <- sum(X[z[i,]==0])
    # number of data belongs to group 1 
    n_2 <- sum(z[i,]==0)
    # standard deviation
    S_2 <- 0.5* sum((X[z[i,]==0]-(X_2/n_2))^2)
   
    # posterior distribution of means and precisions
    
    mu_1_n <- rnorm(1, ((X_1 + 2*0)/(n_1 + 2)), 1/ (precision_1_n*(n_1 + 2)))
    precision_1_n <- rgamma(1, 1+(n_1/2), 1/4 + S_1 + (2*n_1*((X_1/n_1)-0)^2/(2*(2+n_1))))
    
    mu_2_n <- rnorm(1, ((X_2 + 3*1)/(n_2 + 3)), 1/ (precision_2_n*(n_2 + 3)))
    precision_2_n <- rgamma(1, 1+(n_2/2), 1/5 + S_2 + (3*n_2*((X_2/n_2)-1)^2/(2*(3+n_2))))
    
    mu_1_mat[i] <- mu_1_n
    precision_1_mat[i] <- precision_1_n
    
    mu_2_mat[i] <- mu_2_n
    precision_2_mat[i] <- precision_2_n
    
  }
  
  return(list("mu_1"=mu_1_mat, "mu_2"=mu_2_mat, "precision_1"=precision_1_mat, "precision_2"=precision_2_mat, "z"= z, "pi" = pi_mat ))
}


# data
data <- read.csv("G:/My Drive/PhD/Term 7 - Spring 2018/Courses/Statistical Modeling 2/Section 5/restaurants.csv", header = T)
X <- scale(data[,'Profit'])

results <- Gibbssample(X, 10000)

# plots
par(mfrow = c(3,2))

plot(results$mu_1, type = "l")
plot(results$mu_2, type = "l")
plot(results$precision_1, type = "l")
plot(results$precision_2, type = "l")
plot(results$pi)

#Posterior means
mean_cluster_1 <- mean(results$mu_1)
mean_cluster_2 <- mean(results$mu_2)

precision_cluster_1 <- mean(results$precision_1)
precision_cluster_2 <- mean(results$precision_2)
