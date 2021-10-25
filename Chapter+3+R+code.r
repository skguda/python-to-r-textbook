####
### probability4datascience chapter 3 R code translation
### Translated by Sumeeth Guda
####

###
# Libraries to import
###

library(ggplot2)
library(tidyverse)

##
# Chapter 3.2
##

###
# Histogram of the alphabets
###

f <- read.table('./ch3_data_english.txt')
f <- f/100
n <- c(1:26)
ntag <- c('a','b','c','d','e','f','g','h','i','j','k','l','m',
    'n','o','p','q','r','s','t','u','v','w','x','y','z')

f$index <- n
f$labels <- ntag

barplot(f$V1~f$index, xaxt = 'n', xlab = "Letters", ylab = "Values") 
axis(1, at=1:26, labels=ntag)


###
# Histogram of dice throw
###
q <- sample(1:6, 100, replace = T)

hist(q + 0.5, 6)

###
# Histogram of an exponential random number
###

lambda <- 1
k <- 1000
set = runif(n=k, min=0, max=1)
freq = -1/lambda * log(1-set)
hist(freq, 200)

###
# Cross validation loss
###
lambda <- 1
n <- 1000
X <- runif(n=n, min=0, max=1)
freq = -1/lambda * log(1-X)

m <- c(5:200)
J <- replicate(195, 0)

for (i in 1:195) {
    h <- n/m[i]
    J[i] = (2/((n-1)*h)-((n+1)/((n-1)*h)))*sum((hist(freq, m[i])/n)^2)
}

plot(J, m)


###
# Mean of a vector unif(0,1)
###
X <- runif(n=10000, min=0, max=1)
print(mean(X))

###
# Mean of a PMF value
###
p = c(0.25, 0.5, 0.25)
x = c(0, 1, 2)
EX = sum(p*x)
print(EX)

###
# Mean of a geometric random variable
###

k = c(1:100)
p = 0.5 ^ k
EX = sum(p*k)
print(EX)


##
# Chapter 3.5 Common discrete random variables
##

###
# Bernouli random variables histogram
###
p <- 0.5
n <- 1
X <- rbinom(1000, n, p)
hist(X)

###
# Binomial random variables histogram
###

p <- 0.5
n <- 10
X <- rbinom(5000, n, p)
hist(X)

###
# Binomial CDF histogram
###

p <- 0.5
n <- 10
x <- 0:n
plot(x, pbinom(x, size = n, prob = p), type="h")

###
# Poisson-Binomial Approximation
###
n <- 5000
p <- 0.01
x <- rbinom(10000, 5000, 0.01)
pois <- ppois(n*p, x)
plot(x, pois)


###
# Photon shot noise 
###

### Wasn't able to get the image processing packages to load in my dev environment. 
