
titanic <- read.csv("C:/Users/Sareh/Documents/GitHub/sds383d/data/titanic.csv")
titanic <- titanic[!is.na(titanic$Age), ] # remove missing age rows

x <- matrix(titanic$Age, nrow(titanic), 1)
y <- as.numeric(titanic$Survived == "Yes")

log.posterior.function <- function(beta, X, y) {
  - 0.5 * t(beta) %*% beta + y %*% log(1/(1+exp(-X %*% beta))) + (1 - y) %*% log(1 - (1/(1+exp(-X %*% beta))))
}

map <- optim(0, function(beta) -log.posterior.function(beta, x, y), method = "Brent", lower = -1, upper = 1)
map$par
 ## the optimal value of beta ia -0.011


### Problem 3.3

Betarange = -1:1