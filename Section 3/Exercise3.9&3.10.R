
#required library

library(rstan)

tea <- read.csv("C:/Users/Sareh/Documents/GitHub/sds383d/data/tea_discipline_oss.csv", stringsAsFactors = F)
tea <- tea[tea$ACTIONS > 0, ]

# model including one covariate - Grade

# data
teadata <- list(N = nrow(tea), 
             intercept = rep(1, nrow(tea)),
             x = as.numeric(tea$GRADE), 
             y = tea$ACTIONS)

# stan model
stanmodel <- stan(file = 'poisson.stan', data = teadata, chains = 3, iter = 2000)
names(stanmodel) <- c("Intercept", "Grade", "lp__")

# get the outputs of the model
summary(stanmodel)
traceplot(stanmodel)


################################################################################
### including more covariates in model

# data
#include grade, sex, and ethnic as covariates
X <- as.matrix(data.frame(intercept = rep(1, nrow(tea)),
                          grade = as.numeric(tea$GRADE),
                          sex = as.numeric(tea$SEX == "MALE"),
                          hispanic = as.numeric(tea$ETHNICX == "Hispanic/Latino"),
                          black = as.numeric(tea$ETHNICX == "Black or African American"),
                          white = as.numeric(tea$ETHNICX == "White")))

y <- tea$ACTIONS
newdata <- list(N = nrow(X), A = ncol(X), X = X, y = y)



stanmodel2 <- stan(file = 'multivariatepoisson.stan', data = newdata, chains = 3, iter = 2000)
names(stanmodel2) <- c("Intercept", "Grade", "Male", "Black", "Hispanic", "White", "Attendance", "lp__")
summary(stanmodel2)
traceplot(stanmodel2)

#############################################################################
##### including interaction terms


library(rstan)

#data
tea <- read.csv("C:/Users/Sareh/Documents/GitHub/sds383d/data/tea_discipline_oss.csv", stringsAsFactors = F)
tea <- tea[tea$ACTIONS > 0, ]

grade = as.numeric(tea$GRADE)
sex = as.numeric(tea$SEXX == "MALE")

X <- as.matrix(data.frame(intercept = rep(1, nrow(tea)),
                          grade, sex, Interaction = grade*sex))

y <- tea$ACTIONS
newdata <- list(N = nrow(X), A = ncol(X), X = X, y = y)



stanmodel3 <- stan(file = 'multivariatepoisson.stan', data = newdata, chains = 3, iter = 2000)
names(stanmodel3) <- c("Intercept", "Grade", "Sex", "Interaction", "lp__")
summary(stanmodel3)
traceplot(stanmodel3)