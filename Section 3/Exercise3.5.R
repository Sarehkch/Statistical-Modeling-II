
## Exercise 3.5

#define logit function
sig <- function(x) 1 / (1 + exp(-x))

#define log of unnormalized posterior of beta
log.function <- function(beta, X, y) {
  y %*% log(sig(X %*% beta)) + (1 - y) %*% log(1 - sig(X %*% beta)) - 0.5 * t(beta) %*% beta
}

# define gradient function to use in MAP Estimation
gr <- function(beta, X, y) {
  t(X) %*% (y - sig(X %*% beta)) - beta
}

#define Hessian function

H <- function(beta, X, y) {
   - t(X) %*% diag((sig(X %*% beta) * (1 - sig(X %*% beta)))[, 1]) %*% X - diag(1, ncol(X), ncol(X))
}

#import data
data <- read.csv("C:/Users/Sareh/Documents/GitHub/sds383d/data/titanic.csv")
data <- titanic[!is.na(titanic$Age), ] # remove missing age rows


X <- as.matrix(cbind(rep(1, nrow(data)), data$Age))
y <- as.numeric(data$Survived == "Yes")

# mean of estimated posterior distribution of beta
map <- optim(c(0, 0), function(beta) -log.function(beta, X, y), method = "L-BFGS", 
             gr = function(beta) -gr(beta, X, y))

mean = map$par

# Covariance matrix of estimated posterior distribution of beta
cov <- solve(-(H(mean, X, y)))

# 95% credible interval 
CI1 <- mean - 1.96*sqrt(diag(cov))
CI2 <- mean + 1.96*sqrt(diag(cov))

