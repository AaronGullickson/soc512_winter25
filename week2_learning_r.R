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
# you can't do math on words!
#b*2
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
gender_fctr <- factor(gender)
summary(gender)
summary(gender_fctr)
as.numeric(gender_fctr)

# specify levels to control ordering and categories selected
gender_fctr <- factor(gender,
                      levels = c("woman", "man"))
summary(gender_fctr)

# you can also apply custom labels
gender_fctr <- factor(gender,
                      levels = c("woman", "man"),
                      labels = c("W", "M"))
summary(gender_fctr)

# Lists

my_list <- list(name, age, height, likes_soccer, gender_fctr)
# referencing list items by index
my_list[[3]]

# named lists are better
my_list <- list(name = name, age = age, height = height,
                likes_soccer = likes_soccer,
                gender = gender_fctr)
my_list$height
my_list$age
mean(my_list$height)

# data.frame
my_df <- data.frame(name = name, age = age, height = height,
                    likes_soccer = likes_soccer,
                    gender = gender_fctr)

mean(my_df$likes_soccer)
summary(my_df)

# referencd by indices
my_df[3, 4]
my_df[,4]
my_df[,"likes_soccer"]
my_df$likes_soccer


# Boolean Statements ------------------------------------------------------

# == equal to
# != not equal to
# < less than
# > greater than
# <= less than or equal to
# >= greater than or equal to

my_df$age >= 18
my_df$height >= 69
my_df$gender == "M"
my_df$gender != "M"
!my_df$likes_soccer

# compound statements
# & - AND
# | - OR

my_df$likes_soccer & my_df$gender == "M"
my_df$likes_soccer | my_df$gender == "M"

(my_df$likes_soccer | my_df$gender == "M") &
  my_df$age >= 50

my_df$likes_soccer | (my_df$gender == "M" &
  my_df$age >= 50)

# Using Functions ---------------------------------------
mean(my_df$age, na.rm = TRUE)


