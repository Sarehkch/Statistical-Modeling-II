
rm(list=ls())

#Exercise 3.7

#define log of unnormalized posterior of beta
log.function <- function(beta, X, y, lambda) {
  -sum(exp(X %*% beta)) + t(y) %*% X %*% beta - lambda * t(beta) %*% beta
}


# Hessian
  
H<- function(beta, X, y, lambda) { 
-t(X) %*% diag(as.vector((exp(X %*% beta)))) %*% (X) - lambda * diag(1, ncol(X), ncol(X))
}

## diag vector, return a square diagonal matrix  with the elements of vector on the main diagonal
  
#importing data

data <- read.csv("C:/Users/Sareh/Documents/GitHub/sds383d/data/tea_discipline_oss.csv")


# removing -99 from data frame
newdata <- subset(data, ACTIONS !=-99)


# x and y values

y <- as.numeric(newdata$ACTIONS)
X <- as.matrix(cbind(rep(1, nrow(newdata)), newdata$GRADE))

# MAP 
map <- optim(c(0, 0), function(beta) -log.function(beta, X, y,1), method = "L-BFGS")

# Mean of estimated posterior distribution of beta
mean = map$par

# Covariance of estimated posterior distribution of beta
cov <- solve(-H(mean, X, y, 1))

# 95% credible interval 
CI1 <- mean - 1.96*sqrt(diag(cov))
CI2 <- mean + 1.96*sqrt(diag(cov))