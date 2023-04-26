#-------------------------------------------------------------------------------
# First lecture

## Random experiment/random variable

rnorm(n = 1)

# Repeat!

repetitions <- 1000

y <- rnorm(n = repetitions)


# What percentage of values is <= -1 ?
mean(y <= -1)

# What percentage of values is >= 1 ?
mean(y >= 1)

# What percentage of values is in (-1, 1) ?
mean(-1 < y & y < 1)

# each time you run this it will return different values
rnorm(n=1)

# we can run it for 1000 times to see if we get any patterns
y <- rnorm(n=1000)
y

# what percentage of values is <= -1?
sum(y <= -1)/1000 # or like this:
mean(y <= -1)

# what percentage of values is >= 1?
sum(y >= 1)/1000 # or like this:
mean(y >= 1)

# what percentage of values is in (-1, 1)
mean(-1< y & y < 1)

# Now increase the sample size:

repetitions <- 100000
# what percentage of values is <= -1?
sum(y <= -1)/repetitions # or like this:
mean(y <= -1)

# what percentage of values is >= 1?
sum(y >= 1)/repetitions # or like this:
mean(y >= 1)

# what percentage of values is in (-1, 1)
mean(-1< y & y < 1)

# with 68% certainity we know it lies between -1 and 1! to converge to this we 
# need to increase the number of repetitions!


# ------------------------------
# second lecture

##Random experiment/random variables
rnorm(n = 1)

#Repeat
repetitions <- 1000000
y <- rnorm(n = repetitions)
y
hist(y, breaks = 100) ## distribution of y

##overlay probability density function (PDF)
std_norm <- function(y) {
  1/ sqrt(2* pi) * exp(-(y^2 / 2))
}
std_norm(0)
hist(y, freq = FALSE, breaks = 100)

xvals <- seq(-4, to = 4, by = 0.1)
y_den <- std_norm(xvals)
lines(x = xvals, y = y_den, col = "blue", lwd = 2)

xvals # x-values ranging from -4 to 4
y_den # densities corresponding to each x-value

#Built-on R base normal distribution
dnorm(0, mean = )
####Poisson distribution = does not return negative outcome values.
repetitions <- 10000

#Simulate some data
y <- rpois(n = repetitions, lambda = 2.2) # a 100 simulated outcomes
hist(y, breaks = seq(from = 0 - 0.5, to = max(y) + 0.5)) #makes it discrete as it should be

#Overlay the probability distribution ----
d_poisson <- function(y, lambda = 2.2) {
  exp(- lambda) * lambda^y / factorial(y)
}

hist(y, freq= FALSE, breaks = seq(from = 0 - 0.5, to = max(y) + 0.5))
xvals <-0:6
y_den <- d_poisson(xvals)
lines(x = xvals, y = y_den, col = "blue", lwd =2, type = "h")
xvals
y_den

d_poisson(1, lambda = 2.2) # to solve the assignment 1.2 q:3

#built-in base R Poisson function
dpois(1, lambda = 2.2)

##What percentage of values is <= -1?
mean(y <= -1)


##What percentage of values is >= 1?
mean(y >= 1)


##What percentage of values is in (-1, 1)?
mean(-1 < y & y < 1) ## The probability is about 68 percent

##Through this process, we can narrow the range of outcomes down depending on the the values we created.
#Yet there will always be some degrees of uncertainties in our predictions
#but some things might be more likely to occur than others, based on the observations.




#---------------------------------------------------------
#Tutorial lesson 1.1 - 19/04/2023
## Example 1.2

#1) Write code to write the two input a and b?

a <- 4:6 ##Vector
b <- 2 #Scalar

#The summation sign is adding up the intermediate results
##The summation formula
sum(a+b)

#or C++ more complicated way, called "looping"  <- not recommended
seq_along(a)
s <- 0
for (i in seq_along(a)) {
  s <- s + a[i] + b
}
s

# Or this more R compatible way in looping
s <- 0
for (a_i in a) {
  s <- s + a_i + b
}
s

##Example 2.2
p <-8:9
q <- 7
prod(p + q)

#Or this way if the "p" vector has to elements
m <- (p[1] + q) * (p[2] + q)
m


##Write one line of code to compute the scalar product shown in section 5 of the math refresher:
#Use these as inputs
#Example 5

x <- c(1, 3, 2.5, 5, 3)
beta <- c(0.5, 1.3, 0.7, 0.5, 0.1)

##not recommended at all!!!
n <- x[1] * beta[1] + x[2] * beta[2] + x[3] * beta[3]
+ x[4] * beta[4] + x[5] * beta[5]
n

# we can use the looping method instead
for (i in seq_along(x)) {
 xb <- x[i] * beta[i]
}
sum(x * beta)

# or this method but still not recommended!!! 
sum(x * beta)

#preferred solution by "matrix calculation"
x %*% beta
#or transposing the matrix by using the t() function
t(x) %*% beta


##Write a function sq() that takes a single (numeric) input
#and returns the square (of all elements) of the input

sq <- function(x) x^2 
sq(5)

#or for squaring a vector
sq(c(2, 5, 9))

#or
sq <- function(x) {
  out <- x^2
  return(out)
}
sq(5)


##How to do a linear normal DGP (data generating process??
n <- 1000
beta_1 <- 1

#Create x vector in the simulation ....
x <- seq(from = 0, to = 1, length.out = n)
x
#or display x in a data frame where X is the "explanatory variable"
data.frame(x)

#now we want another column to display the "dependent variable" which is a stochastic variable
#The mean is a function of "x" and "beta" and it is fixed.

# Simulation data y ....

#systematic component
xb <- x * beta_1

#stochastic component
y <- rnorm(n = n, mean = xb) ##remember that "rnorm" is a randomly distributed variable

data.frame(x, y) #we computed 1000 means in this case

##scatter plot
plot(x, y)
abline(c(0, beta_1))

#This is the linear regression model, we can repeat it many times to see how the values deviate
lm(y ~ x)


#homework, generalize this to the case where beta <- (beta_1, beta_2)
##such as abline(beta)



# ----------------------------------------------------------------------------
# Second tutorial:

# missing part
d_bionomial <- function(y, N, p){
  choose(N, y) * p^y * (1-p)^(N-y)
}

y_all_female <- 10
y_all_male <- 0

d_bionomial(y_all_female, N=10, p=0.4)
d_bionomial(y_all_male, N=10, p=0.4)


# when you have mutually exclusive outcomes like nbr of females in the cabinet, you 
# can add up the probability of having 0 female, 1 female, 2 females, etc.

Nbr <- 10
prob <- 0.4

y_maj_male <- 0:4
y_maj_female <- 6:10

sum(d_bionomial(y_maj_female, N=Nbr, p=prob))
sum(d_bionomial(y_maj_male, N=Nbr, p=prob))

x_vals <- 0:10
pr_y <- d_bionomial(x_vals, N=Nbr, p=prob)
plot(x= x_vals, y=pr_y, type='h', lwd=2)


# the built-in fuction for bionomial distribution is dbinom

dbinom(x = 3, size = 10, prob = 0.4)


sum(dbinom(x= y_maj_female, size=Nbr, prob = prob))
sum(dbinom(x=y_maj_male, size=Nbr, prob=prob))

# or better still, we are using the cumulative distribution functions:
pbinom(4, size=10, prob =0.4)
pbinom(5, size=10, prob=0.4, lower.tail = FALSE)

