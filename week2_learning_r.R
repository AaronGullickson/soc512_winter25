##################
# Aaron Gullickson
# Soc 412/512
# Winter 2025
# Week 2
##################

# This is a comment more than a question This is a comment more than a question
# This is a comment more than a question

# Basic Object Types ------------------------------------------------------

# numeric - double or integer
a <- 2

# character (string)
b <- "bob"

# logical
c <- TRUE

# mathematical operations
a*2
b*2
c*2

as.character(a)
as.character(c)
as.numeric(c)
as.numeric(b)
as.numeric("2")

# Vector

name <- c("bob", "harry", "sally", "lars", "musashi", "jennifer")
gender <- c("man", "man", "woman", "man", "man", "woman")
age <- c(23, 37, NA, 87, 42, 37)
height <- c(73, 67, 64, 75, 68, 67)
likes_soccer <- c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE)

# you can't combine different types
c(4, "b", TRUE)

mean(height)
mean(likes_soccer)
mean(age, na.rm = TRUE)

# indexing vectors
height[3]
height[c(1, 4)]
height[3:5]

# matrices
x <- cbind(age, height)

# indexing matrices
x[3, 2]
x[, "age"]
mean(x[,"height"])

cbind(name, age, height, likes_soccer)

# factors
