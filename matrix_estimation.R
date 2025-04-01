library(tidyverse)
load("class_data/movies.RData")

X <- as.matrix(cbind(rep(1, nrow(movies)), movies[,c("runtime","box_office")]))
y <- movies$metascore

# matrix multiply X by X
t(X)%*%X

# simple way to matrix multiply X by X
crossprod(X)

crossprod(X, y)

# calculate the inverse of a matrix with "solve"
solve(crossprod(X))

# A matrix multiplied by its inverse gives you the identity matrix
round(solve(crossprod(X)) %*% crossprod(X), 10)

# full thing
beta <- solve(crossprod(X)) %*% crossprod(X, y)
beta

coef(lm(metascore ~ runtime + box_office, data = movies))

