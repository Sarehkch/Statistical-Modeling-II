
#required library

library(MCMCpack)

#sample dirichlet distribution

pi = matrix(NA, nrow = 5, ncol=100)

z = matrix(NA, nrow = 5, ncol=10)

for (i in 1:5){

  pi[i,] <- rdirichlet(1,c(rep(10,100)))
  z[i,] <- sample(1:100, size =10, prob = pi[i,], replace = T) 
}

rownames(z) <- c("run1", "run2", "run3", "run4", "run5")


###########################################
# repeat with alph = (1,1,...,1)

pi = matrix(NA, nrow = 5, ncol=100)

z = matrix(NA, nrow = 5, ncol=10)

for (i in 1:5){
  
  pi[i,] <- rdirichlet(1,c(rep(1,100)))
  z[i,] <- sample(1:100, size =10, prob = pi[i,], replace = T) 
}

rownames(z) <- c("run1", "run2", "run3", "run4", "run5")


###########################################
# repeat with alph = (0.1,0.1,...,0.1)

pi = matrix(NA, nrow = 5, ncol=100)

z = matrix(NA, nrow = 5, ncol=10)

for (i in 1:5){
  
  pi[i,] <- rdirichlet(1,c(rep(0.1,100)))
  z[i,] <- sample(1:100, size =10, prob = pi[i,], replace = T) 
}

rownames(z) <- c("run1", "run2", "run3", "run4", "run5")

###########################################
# repeat with alph = (0.01,0.01,...,0.01)

pi = matrix(NA, nrow = 5, ncol=100)

z = matrix(NA, nrow = 5, ncol=10)

for (i in 1:5){
  
  pi[i,] <- rdirichlet(1,c(rep(0.01,100)))
  z[i,] <- sample(1:100, size =10, prob = pi[i,], replace = T) 
}

rownames(z) <- c("run1", "run2", "run3", "run4", "run5")

##################
#plots

par(mfrow = c(2,2))

pie = rdirichlet(1,c(rep(10,100)))
plot(sort(pie))
title(main = "alpha = 10")

pie = rdirichlet(1,c(rep(1,100)))
plot(sort(pie))
title(main = "alpha = 1")

pie = rdirichlet(1,c(rep(0.1,100)))
plot(sort(pie))
title(main = "alpha = 0.1")

pie = rdirichlet(1,c(rep(0.01,100)))
plot(sort(pie))
title(main = "alpha = 0.01")