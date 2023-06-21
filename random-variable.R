#-------------------------------------------------------------------------------
# First lecture 17.4

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
# second lecture 24.4

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
# Second tutorial: 26.4

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

#-----------------------------------------------------------------
#Third session 8.5

# Assignment 2.1 question 5
#hypothesis parameter valus
lambda_hyp1 <- 1.1
lambda_hyp2 <- 2.9
lambda_hyp3 <- 3.5

#data
y <-2

#likelihood:
dpois(x=y, lambda = lambda_hyp1)
dpois(x=y, lambda = lambda_hyp2)
dpois(x=y, lambda = lambda_hyp3)

#second way of doing this task:
lambda_hyp <- c(1.1, 2.9, 3.5)
lambda_tilde <- setNames(nm = lambda_hyp)
dpois(x=y, lambda = lambda_tilde)

#plot lilelihood functions
lambda_tilde <- seq(from=0, to=10, by=1e-2)
l_pois <- dpois(x=y, lambda = lambda_tilde, log = FALSE)
l_pois_log <- dpois(x=y, lambda = lambda_tilde, log = TRUE)
plot(x=lambda_tilde, y=l_pois, type='l')
abline(v=2, col='red')
plot(x=lambda_tilde, y=l_pois_log, type='l')
abline(v=2, col='red')

1e-2

#second example
y<-4
N<-15
p_tilde <- seq(from=0, to=1, by=1e-2)
l_binom <- dbinom(x=y, size = N, prob = p_tilde)
plot(x=p_tilde, y=l_binom, type='l')
abline(v=0.27, col='pink')
abline(v=4/15, col='red')
#final point
# taking the log doesn't change the density function????? Ask!

#end
#_________________________________________________________________________________________
#Third tutorial  (was canceled)
#____________________________________________________________________________________________
#forth session lesson 2.2 15.5
# Normal (maximum) likelihood with fixed sigma

y <- c(3.4, 1.2, 0.8, 0.9, 2.2, 1.5, 2.3)

# Analytical MLE of mu

mle_ana <- mean(y)

#check the values generated by normal distribution for 
dnorm(y, mean = 2, sd=1, log=FALSE)
dnorm(y, mean = 2, sd=1, log = TRUE)

# the log likelihood score is the sum over the probabilities???
sum(dnorm(y, mean = 3, sd=1, log = TRUE))

# Graphical MLE of mu
# we ignore the sigma factor according to THE BOOK! knowing one would not help to know the other!
ll_norm <- function(mu, y){  #log likelihood of normal distribution
  sigma <- 1 #King calls it stylized normal distribution
  ll <- sum(dnorm(y, mean = mu, sd=sigma,  log = TRUE))
  return(ll)
}

# lets try it out
ll_norm(0, y)
ll_norm(1.3, y)
ll_norm(2.2, y)
ll_norm(2.9, y)

mu_tild <- seq(from = -1, to = 6, by =1e-2)

#create an empty list of numbers (vector?)
logl <- numeric()

# filling logl element by element
for ( i in seq_along(mu_tild)){
  logl[i] <- ll_norm(mu= mu_tild[i], y)
}

logl
# see as dataframe
data.frame(mu_tild, logl)

plot(mu_tild, logl, type="l")
abline(v=mle_ana, col='red')
mle_ana


#________________________________________________________________________________]
# 22.05.2023 Fifth session

# normal (maximum) likelihood with fixed sigma

# we have seen that the mean is the best guess for the maximum likelihood
# numerical MLE

# the key fcuntcion here is the optim() function, this code is written by john nash himself :D
# we use ll_norm function we defined during the forth week
theta_start <-0
# following function will return a warning and the result is wrong! because optim by defult 
# returns the minumun not the maximum and indeed it minimized the ll_norm! and ll_norm as we know does not have min!
optim(
  fn = ll_norm,
  par=  theta_start, # we have to start from somewhere and need a starting point
  y = y,
  )

# the fixed version is:
norm_reg <- optim( #basically we ran a simple regression
  fn = ll_norm,
  par=  theta_start, # we have to start from somewhere and need a starting point
  y = y,
  control = list(fnscale=-1), # flips the function around and now we can find the maximum
  method = "BFGS" #not mandatory parameter, but it will get rid of the warning
)
# again we get a warning but we ignore the warning! 
norm_reg

mle_num <- norm_reg$par
# to check how similar/different the analytical and numerical mle are:
mle_num
mle_ana


# computing the standard errors:
# using the analytic second derivative:

h_ana <- -length(y) #the hessian

#varicance covariance matrix
vrcov <- - (h_ana)^(-1)

#standard error
se <- sqrt(vrcov)
se

# using numerical second derivative
theta_start <- 0
norm_reg <- optim(
  fn=ll_norm,
  par=theta_start,
  y=y,
  control = list(fnscale=-1),
  method="BFGS",
  hessian=TRUE
  )
h_num <-norm_reg$hessian

vcov <- -(solve(h_num)) # inverse of the matrix
#taking the square toot of the diagonal
se <- sqrt(diag(vcov))
se

#we can use the above code for any model! just change the function that we want to optimize, the loglikelihood function, the rest is exactly the same (the optimization steps)


#________________________________________________________________________________
# fifth Tutorial session, 24.05.2023

# Assignment 2.3: 
# Exercise 3:
# we always check the null hypothesis and check if it hold and considering the data, 
# how strong it is.
# To do so we start backward to see what we have and what we are missing. To do so use the likelihood ratio test
# to see if the hypothesis is compatible with the data and how compatible it is 
# recall that we have log L*/L*_R thus the graph looks like 1/x and depending on where it lies we can reject or accept the hypothesis
# we fixed the parameter at the fixed value anc check with the support that the data gives if we allow the data causes the value to vary (this is the un-restricted model)
# we have to figure out 2 quantities, the log likelihood at the maximum? look at the log likelihood function

# the critical value of the chi-square:
qchisq(0.05, df=1, lower.tail = FALSE) # check this function,
qchisq(0.95, df=1) # the same value
# the df, the degree of freedom is set ot 1 as we have one restriction on our parameter
# in 3.b we have more restriction thus df should be higher???

# ----------------------------------------------------------------------------------------
#June 11th lecture / sixth session

#see lecture notes for the formula
# maximum likelihood, normal y with predictor
# example
# regression of government approval on unemployment.

path_data <- 'Developement/RD2/approval-kohl.csv'
#loading the data
data <- read.csv(path_data)
# look at the data
data

yvar <- "approve"
xvars <- "unemp"

y <- data[, yvar]
x <- as.matrix(data[, xvars])

theta_start <- c(0, 0, 1)

# log likelihood function for stylized normal 
ll_norm <- function(mu, y, x=NULL){  #log likelihood of normal distribution, notice here x has been set to null, check why!
  n_par <- length(theta)
  beta <- theta(n_par)
  mu <- cbind(1, x) %*% beta
  sigma <- 1 #King calls it stylized normal distribution
  ll <- sum(dnorm(y, mean = mu, sd=sigma,  log = TRUE))
  return(ll)
}

# estimation ... 
norm_reg <- optim( #basically we ran a simple regression
  fn = ll_norm,
  par=  theta_start, # we have to start from somewhere and need a starting point
  y = y,
  control = list(fnscale=-1), # flips the function around and now we can find the maximum
  method = "BFGS", #not mandatory parameter, but it will get rid of the warning
  hessian = TRUE
)



#------------------------------------------------------------------------------------
# tutorial 21.06

n_obs <- 5
max_obs <- 53

#likelihood function ...
if_tanks <- function(N_tanks, n_obs, max_obs){
  ifelse(
    max_obs <= N_tanks,
    1/ choose(N_tanks, n_obs),
    0
  )
}

# plotting
N_tanks_tilde <- seq(from=1, to=max_obs+20)
l_tanks <- if_tanks(N_tanks_tilde, n_obs, max_obs)
plot(N_tanks_tilde, l_tanks, type="h")


# how to calculate the critical value
qnorm(0.95)
qnorm(0.05, lower.tail = FALSE)


