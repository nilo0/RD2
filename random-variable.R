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


# ------------------------------------------------------------------------------
#June 26th

## ML estimation of Bernoulli y with predictors

## Bundestag vote on same-sex marriage
## MPs from CDU/CSU


# define data and variables ----

yvar <- "yes"
xvars <- c(
  "female",
  "age",
  "list_only",
  "votes_union",
  "votes_green"
)

path_data <- "~/Downloads/cdu-marriage.csv"


# log-likelihood function for Bernoulli regression ----

ll_bern <- function(theta, y, x = NULL) {
  
  beta <- theta
  
  if (is.null(x))
    xb <- beta
  else 
    xb <- cbind(1, x) %*% beta
  
  p <- 1 / (1 + exp(-xb))
  
  ll <- sum(dbinom(y, size = 1, prob = p, log = TRUE))
  
  return(ll)
  
}


# estimation inputs ----

data <- read.csv(path_data)

y <- data[, yvar]
x <- as.matrix(data[, xvars])

theta_start <- setNames(
  rep_len(0, ncol(x) + 1),
  nm = c("const", xvars)
)


# estimation ----

regres <- optim(
  fn = ll_bern,
  par = theta_start,
  y = y,
  x = x,
  control = list(fnscale = -1),
  method = "BFGS",
  hessian = TRUE
)


# post estimation ----

# standard errors ----

h <- regres$hessian
vcov <- -(solve(h))
se <- sqrt(diag(vcov))


# Wald test statistics for H_0: k = 0 ----

k <- 0

coefs <- regres$par

w <- (coefs - k) / se
p_val <- pnorm(abs(w), lower.tail = FALSE) * 2


# print result ----

round(
  data.frame(Coef = coefs, SE = se, z = w, p = p_val),
  digits = 4
)

 
 
 #compute fitted values
 x <- c(
   female=0,
   age=5,
   list_only=0,
   voters_union=0.35,
   voters_green=0.08
 )
 
 # compute expected value of Y, giben estimates of beta
 xb <- c(1, x)%*%coefs 
 pr <- 1 / (1 + exp(-xb))
 
 # note that we need to have c(1, x) for the matrix multiplication, 1 here would be multiplied by the constant coeff of the regression model
 
 
 #compute the first difference
 
 # input values
 x_0 <- c(
   female=0,
   age=5,
   list_only=0,
   voters_union=0.35,
   voters_green = 0.08
 )

 x_1  <- c(
   female=0,
   age=4,
   list_only=0,
   voters_union=0.35,
   voters_green = 0.08
 )
 
 xb_0 <- c(1, x_0)%*%coefs
 xb_1 <- c(1, x_1)%*%coefs

 pr_0 <- 1 / (1 + exp(-xb_0))
 pr_1 <- 1 / (1 + exp(-xb_1))

 fd <- pr_1 - pr_0
 fd
 # the sign is positive as it should be as the coeff of age is negative thus decreasing the age must lead to positive first difference due to change in age.
 # now if we change one other variable and calculate the first difference the result is superficially similar but different a bit, but in general (not like this example)
 # the change could be much higher, in this case as the gender has small effect (considering z, p and smalll fd) change in fd due to change in gender is small 
 # now if we consider a variable with significant effect on the outcome then the change in fd due to change in this variable is high! 
 
 
 
 # calculate quantities of interest with uncertainity ...
 # estimate via parametric bootstrap
 
 n_boot <- 1000 #1000 is usually good value for this kind of simulation
 seed <-1234
 # sample potential coefficinents, coefficients we could have obtained with this kind of sample
 
 coefs_boot <- MASS::mvrnorm(n=n_boot, mu=coefs, Sigma = vcov) #multi variate random variable from normal distribution, not part of based R and need MASS package
 #to check if mass is installed
 library(MASS)
# look at the coefs_boot
 coefs_boot
  

 xb_boot <- coefs_boot%*%c(1, x) # we just need to change it around due to matrix multiplication
pr_boot <- 1/ (1 + exp(-xb_boot))
pr_boot
 df <- data.frame(coefs_boot, xb_boot, pr_boot)
head(df) 

# now the first differences
xb_boot_0 <- coefs_boot%*%c(1, x_0)
xb_boot_1 <- coefs_boot%*%c(1, x_1)

pr_boot_0 <- 1/ (1 + exp(-xb_boot_0))
pr_boot_1 <- 1/ (1 + exp(-xb_boot_1))

fd_boot <- pr_boot_1 - pr_boot_0

mean(fd_boot)
quantile(fd_boot, prob=c(0.025, 0.975))

#check the code he uploads


#----------------------------------------------------------------------------------
#June 28th tutorial

# to calculate the first differences we need to set the parameters to some specific value
# and by changing the desired parameter, we obtain the first difference. But how should we 
# choose values for the other parameters? In research approach often they choose the mean value 
# for this parameters, to avoid choosing some extreme values for these parameters
 
# "At the average" approach -----
# compute fitted value -----
#input values -------

# age:= 50
# others := average
#my own code
x_avg_0  <- c(
  female=mean(data$female),
  age=5,
  list_only=mean(data$list_only),
  voters_union=mean(data$votes_union),
  voters_green = mean(data$votes_green)
)
x_avg_0 <- c(1, x_avg_0)%*%coefs
pr_avg_0 <- 1/ (1 + exp(-x_avg_0))
pr_avg_0

# his solution
var <- "age"
value <- 5
x_avg <- colMeans(data[, xvars])
x <- replace(x_avg, list = var, values = value)
xb <- coefs%*%c(1, x)
pr <- 1 / (1 + exp(-xb))
mean(pr)
quantile(pr, prob=c(0.025, 0.957))

# now calculate the first difference when the age is for ages 50 - 40
x_avg <- colMeans(data[, xvars])
var <- "age"
value <- c("x_0" = 5, "x_1"=4)

x_0 <- replace(x_avg, list = var, values = value['x_0'])
x_1 <- replace(x_avg, list = var, values = value['x_1'])

xb_0 <- coefs%*%c(1, x_0)
xb_1 <- coefs%*%c(1, x_1)

pr_0 <- 1 / (1 + exp(-xb_0))
pr_1 <- 1 / (1 + exp(-xb_1))

fd <- pr_1 - pr_0
mean(fd)
#the confidence interval would be:
quantile(fd, prob=c(0.025, 0.957))


# Average value approach
# Now we don't consider the parametric bootstraps :D just the first differences
var <- "age"
value <- c("x_0"=5, "x_1"=4)

x_obs <- data[, xvars]
x_obs_0 <- replace(x_obs, list = var, values = value['x_0'])
x_obs_1 <- replace(x_obs, list = var, values = value['x_1'])

xb_obs_0 <- as.matrix(cbind(1, x_obs_0))%*%coefs
xb_obs_1 <- as.matrix(cbind(1, x_obs_1))%*%coefs

pr_obs_0 <- 1/ (1 + exp(- xb_obs_0))
pr_obs_1 <- 1/ (1 + exp(- xb_obs_1))

fd_obs <- pr_obs_1 - pr_obs_0
mean(fd_obs)
quantile(fd_obs, probs = c(0.025, 0.975))

# to have the confidence interval we use parametric bootstrap which in this case is to change the coefs to 
# bootstraps we had earlier as coefs_boot
coefs_boot
# Adding uncertainity estimate (CI) via parametric bootstrap
x_avg <- colMeans(data[, xvars])
var <- "age"
value <- c("x_0" = 5, "x_1"=4)

x_0 <- replace(x_avg, list = var, values = value['x_0'])
x_1 <- replace(x_avg, list = var, values = value['x_1'])
# for each draw we need to pre-allocate it
fd <- numeric()
for (i in seq_len(nrow(coefs_boot))) {
  coefs_draw <- coefs_boot[i, ]
  
  xb_draw_0 <- as.matrix(cbind(1, x_obs_0))%*%coefs_draw
  xb_draw_1 <- as.matrix(cbind(1, x_obs_1))%*%coefs_draw
  
  pr_draw_0 <- 1/ (1 + exp(- xb_draw_0))
  pr_draw_1 <- 1/ (1 + exp(- xb_draw_1))
  
  fd[i] <- mean(pr_draw_1 - pr_draw_0)
}

mean(fd)
quantile(fd, probs = c(0.025, 0.975))

# ------------------------------------------------------------------------------
# lecture on July 3rd
# ML estimation of Possion y with predictor

path_data <- '~/Downloads/assoc-memberships.csv'

data <- read.csv(path_data)
data

# define data and variables

yvar <- "assoc"
xvars <- c(
  "female",
  "age",
  "educ",
  "inc",
  "east",
  "tv"
)

#log-likelihood function for possion regression
# the effect role estimate is unkown (beta) and we want to estimate it
ll_pos <- function(theta, y, x=NULL){
  beta <- theta
  if (is.null(x))
    xb <- beta
  else
    xb <- cbind(1, x) %*% beta
  lamb <- exp(xb)
  ll <- sum(dpois(y, lambda = lamb, log = TRUE)) # the log is set to true as we dont want the probabilites, but the log-likelihood probabilities
  return(ll) 
}

y <- data[, yvar]
x <- as.matrix(data[, xvars])


theta_start <- setNames(
  rep_len(0, ncol(x) + 1),
  nm = c("const", xvars)
)


# estimation ----

regres <- optim(
  fn = ll_pos,
  par = theta_start,
  y = y,
  x = x,
  control = list(fnscale = -1),
  method = "BFGS",
  hessian = TRUE
)


# post estimation ----

# standard errors ----

h <- regres$hessian
vcov <- -(solve(h))
se <- sqrt(diag(vcov))


# Wald test statistics for H_0: k = 0 ----

k <- 0

coefs <- regres$par
coefs

w <- (coefs - k) / se
p_val <- pnorm(abs(w), lower.tail = FALSE) * 2


# print result ----

round(
  data.frame(Coef = coefs, SE = se, z = w, p = p_val),
  digits = 2
)


# compute first differece (at average approach)
#input values

var <- "tv"
value <- c("x_0" = 4, "x_1" = 5)

x_avg <- colMeans(data[, xvars])
x_0 <- replace(x_avg, list = var, values = value['x_0'])
x_1 <- replace(x_avg, list = var, values = value['x_1'])

# calculate quantiles of interest with uncertainity (the confidence intervals)
# estimates via parametric bootstrap

n_boot <- 1000
seed <- 1234

# draw bootstarp samples of coefficients
set.seed(seed)
library(MASS)
coefs_boot <- MASS::mvrnorm(n=n_boot, mu=coefs, sigma=vcov)

# compute the first difference and 95% confidence interval

xb_boot_0 <- coefs_boot%*%c(1, x_0) # we just need to change it around due to matrix multiplication
xb_boot_1 <- coefs_boot%*%c(1, x_1)

lamb_boot_0 <- exp(xb_boot_0)
lamb_boot_1 <- exp(xb_boot_1)

fd_boot <- lamb_boot_1 - lamb_boot_0
mean(fd_boot)
quantile(fd_boot, prob=c(0.025, 0.975))


# =============================================================================
#Possion property var(Y) == mean (Y)
lamb <- 2.45
n = 1e7
seed <- 12345

# simulate y
set.seed(seed)

y <- rpois(n, lambda = lamb)

mean(y)
var(y)


# -----------------------------------------------------------------------------------
# tutorial july 5th

path_data <- '~/Downloads/assoc-memberships.csv'

data <- read.csv(path_data)
data

# define data and variables

yvar <- "assoc"
xvars <- c(
  "female",
  "age",
  "educ",
  "inc",
  "east",
  "tv"
)

#log-likelihood function for possion regression
# the effect role estimate is unkown (beta) and we want to estimate it
ll_pos <- function(theta, y, x=NULL){
  beta <- theta
  if (is.null(x))
    xb <- beta
  else
    xb <- cbind(1, x) %*% beta
  lamb <- exp(xb)
  ll <- sum(dpois(y, lambda = lamb, log = TRUE)) # the log is set to true as we dont want the probabilites, but the log-likelihood probabilities
  return(ll) 
}

y <- data[, yvar]
x <- as.matrix(data[, xvars])


theta_start <- setNames(
  rep_len(0, ncol(x) + 1),
  nm = c("const", xvars)
)


# estimation ----

regres <- optim(
  fn = ll_pos,
  par = theta_start,
  y = y,
  x = x,
  control = list(fnscale = -1),
  method = "BFGS",
  hessian = TRUE
)


# post estimation ----

# standard errors ----

h <- regres$hessian
vcov <- -(solve(h))
se <- sqrt(diag(vcov))


# Wald test statistics for H_0: k = 0 ----

k <- 0

coefs <- regres$par

w <- (coefs - k) / se
p_val <- pnorm(abs(w), lower.tail = FALSE) * 2


# print result ----

round(
  data.frame(Coef = coefs, SE = se, z = w, p = p_val),
  digits = 2
)


# Average value approach
# Now we don't consider the parametric bootstraps :D just the first differences
var <- "tv"
value <- c("x_0"=4, "x_1"=5)

x_obs <- data[, xvars]
x_obs_0 <- replace(x_obs, list = var, values = value['x_0'])
x_obs_1 <- replace(x_obs, list = var, values = value['x_1'])

xb_obs_0 <- as.matrix(cbind(1, x_obs_0))%*%coefs
xb_obs_1 <- as.matrix(cbind(1, x_obs_1))%*%coefs

lamb_obs_0 <- exp(xb_obs_0)
lamb_obs_1 <- exp(xb_obs_1)

fd_obs <- lamb_obs_1 - lamb_obs_0
mean(fd_obs)
quantile(fd_obs, probs = c(0.025, 0.975))

# to have the confidence interval we use parametric bootstrap which in this case is to change the coefs to 
# bootstraps we had earlier as coefs_boot

# Adding uncertainity estimate (CI) via parametric bootstrap
x_avg <- colMeans(data[, xvars])
var <- "tv"
value <- c("x_0" = 4, "x_1"=5)


n_boot <- 1000 #1000 is usually good value for this kind of simulation
seed <-1234
# sample potential coefficinents, coefficients we could have obtained with this kind of sample

coefs_boot <- MASS::mvrnorm(n=n_boot, mu=coefs, Sigma = vcov) #multi variate random variable from normal distribution, not part of based R and need MASS package
#to check if mass is installed
library(MASS)
# look at the coefs_boot
coefs_boot

x_0 <- replace(x_avg, list = var, values = value['x_0'])
x_1 <- replace(x_avg, list = var, values = value['x_1'])
# for each draw we need to pre-allocate it
fd <- numeric()
coefs_boot
for (i in seq_len(nrow(coefs_boot))) {
  coefs_draw <- coefs_boot[i, ]

  xb_draw_0 <- as.matrix(cbind(1, x_obs_0))%*%coefs_draw
  xb_draw_1 <- as.matrix(cbind(1, x_obs_1))%*%coefs_draw
  
  lamb_draw_0 <- exp(xb_draw_0)
  lamb_draw_1 <- exp(xb_draw_1)
  
  fd[i] <- mean(lamb_draw_1 - lamb_draw_0)
}

mean(fd)
quantile(fd, probs = c(0.025, 0.975))

# Assessing the possion assumption
mean(y)
var(y)

outcome <- seq(from=min(y), to=max(y))
outcome <- setNames(nm = outcome)

x_obs <- data[, xvars]
lamb <- exp(as.matrix(cbind(1, x_obs))) %*% coefs
pr <- sapply(outcome, dpois, lambda=lamb)
pr_avg <- colMeans(pr)

# compare
round(pr_avg, digits = 3)
round(prop.table(y), digits=3)


#------------------------------------------------------------------------------------


# Lecture 10 July

## ML estimation of Poisson y with predictors

## Number of association memberships: ALLBUS


## ML estimation of negative binomial y with predictors

## Number of association memberships: ALLBUS


# define data and variables ----

yvar <- "assoc"
xvars <- c(
  "female",
  "age",
  "educ",
  "inc",
  "east",
  "tv"
)

path_data <- "data/assoc-memberships.csv"


# log-likelihood function for Poisson regression ----

ll_negbin <- function(theta, y, x = NULL) {
  
  n_par <- length(theta)
  
  beta <- theta[-n_par]
  delta <- theta[n_par]
  
  if (is.null(x))
    xb <- beta
  else 
    xb <- cbind(1, x) %*% beta
  
  phi <- exp(xb)
  sigma2 <- 1 + exp(delta)
  
  ll <- sum(dnbinom(y, size = phi / (sigma2 - 1), mu = phi, log = TRUE))
  
  return(ll)
  
}


# estimation inputs ----

data <- read.csv(path_data)

y <- data[, yvar]
x <- as.matrix(data[, xvars])

theta_start <- setNames(
  rep_len(0, ncol(x) + 2),
  nm = c("const", xvars, "delta")
)


# estimation ----

regres <- optim(
  fn = ll_negbin,
  par = theta_start,
  y = y,
  x = x,
  control = list(fnscale = -1),
  method = "BFGS",
  hessian = TRUE
)


# post estimation ----

# coefficient estimates ----

coefs <- regres$par

# standard errors ----

h <- regres$hessian
vcov <- -(solve(h))
se <- sqrt(diag(vcov))


# Wald test statistics for H_0: k = 0 ----

k <- 0

w <- (coefs - k) / se
p_val <- pnorm(abs(w), lower.tail = FALSE) * 2


# print result ----

round(
  data.frame(Coef = coefs, SE = se, z = w, p = p_val),
  digits = 2
)


# Calculate quantities of interest with uncertainty ----
# estimates via parametric bootstrap

n_boot <- 1000
seed <- 1234

# draw bootstrap samples of coefficients ----

set.seed(seed)
par_boot <- MASS::mvrnorm(n = n_boot, mu = coefs, Sigma = vcov)

coefs_boot <- par_boot[, c("const", xvars)]


## Compute first difference ("at average" approach) ----

# input values ----

var <- "tv"
value <- c("x_0" = 4, "x_1" = 5)

x_avg <- colMeans(data[, xvars])
x_0 <- replace(x_avg, list = var, values = value["x_0"])
x_1 <- replace(x_avg, list = var, values = value["x_1"])


# compute first difference and 95% confidence interval ----

xb_boot_0 <- coefs_boot %*% c(1, x_0) 
xb_boot_1 <- coefs_boot %*% c(1, x_1) 

lamb_boot_0 <- exp(xb_boot_0)
lamb_boot_1 <- exp(xb_boot_1)

fd_boot <- lamb_boot_1 - lamb_boot_0

mean(fd_boot)
quantile(fd_boot, prob = c(0.025, 0.975))


#---------------------------------------------------------------------------------
# tutorial July 12th
# likelihood raito test of equidespersion

# we estimate each model, possion and negative bionomial

# source() the directoery where the possion model is stored 
# get loglikelihood of each model
source("~/Developement/RD2/ml-negbin-y-x-parboot(1).R")
ll_poisson <- regres$value

source("~/Developement/RD2/ml-poisson-y-x-parboot(1).R")
ll_negbin <- regres$value

# do the test ...
r <- 2 *(ll_negbin - ll_poisson)
r
round(pchisq(r, df=1, lower.tail=FALSE), digits=5)


yvar <- "assoc"
xvars <- c(
  "female",
  "age",
  "educ",
  "inc",
  "east",
  "tv"
)


path_data <- "~/Developement/RD2/assoc-memberships.csv"


# log-likelihood function for Poisson regression ----

ll_negbin <- function(theta, y, x = NULL) {
  
  n_par <- length(theta)
  
  beta <- theta[-n_par]
  delta <- theta[n_par]
  
  if (is.null(x))
    xb <- beta
  else 
    xb <- cbind(1, x) %*% beta
  
  phi <- exp(xb)
  sigma2 <- 1 + exp(delta)
  
  ll <- sum(dnbinom(y, size = phi / (sigma2 - 1), mu = phi, log = TRUE))
  
  return(ll)
  
}


# estimation inputs ----

data <- read.csv(path_data)

y <- data[, yvar]
x <- as.matrix(data[, xvars])

theta_start <- setNames(
  rep_len(0, ncol(x) + 2),
  nm = c("const", xvars, "delta")
)


# estimation ----

regres <- optim(
  fn = ll_negbin,
  par = theta_start,
  y = y,
  x = x,
  control = list(fnscale = -1),
  method = "BFGS",
  hessian = TRUE
)


# post estimation ----

# coefficient estimates ----

coefs <- regres$par

# standard errors ----

h <- regres$hessian
vcov <- -(solve(h))
se <- sqrt(diag(vcov))


# Wald test statistics for H_0: k = 0 ----

k <- 0

w <- (coefs - k) / se
p_val <- pnorm(abs(w), lower.tail = FALSE) * 2


# print result ----

round(
  data.frame(Coef = coefs, SE = se, z = w, p = p_val),
  digits = 2
)


# Calculate quantities of interest with uncertainty ----
# estimates via parametric bootstrap

n_boot <- 1000
seed <- 1234

# draw bootstrap samples of coefficients ----

set.seed(seed)
par_boot <- MASS::mvrnorm(n = n_boot, mu = coefs, Sigma = vcov)

coefs_boot <- par_boot[, c("const", xvars)]


## Compute first difference ("at average" approach) ----

# input values ----

var <- "tv"
value <- c("x_0" = 4, "x_1" = 5)

x_avg <- colMeans(data[, xvars])
x_0 <- replace(x_avg, list = var, values = value["x_0"])
x_1 <- replace(x_avg, list = var, values = value["x_1"])


# compute first difference and 95% confidence interval ----

xb_boot_0 <- coefs_boot %*% c(1, x_0) 
xb_boot_1 <- coefs_boot %*% c(1, x_1) 

lamb_boot_0 <- exp(xb_boot_0)
lamb_boot_1 <- exp(xb_boot_1)

fd_boot <- lamb_boot_1 - lamb_boot_0

mean(fd_boot)
quantile(fd_boot, prob = c(0.025, 0.975))




outcome <- seq(from=min(y), to=max(y))
outcome <- setNames(nm=outcome)
x_obs <- as.matrix(data[, xvars])
x_obs


# compute predicted Pr.... 
beta <- coefs[c('const', xvars)]
delta <- coefs['delta']

phi<- exp(cbind(1, x_obs)%*% beta)
sigma2<- 1 + exp(delta)

pr<-sapply(outcome, dnbinom, mu=phi, size=phi/(sigma2 - 1))
pr_avg <- colMeans(pr)


#compute observed distribution:
freq <- prop.table(table(factor(y, levels=outcome)))
freq 

# compare predicted to the observed distrubution ....
round(pr_avg, digits=3) #predictions of our model
round(freq, digits = 3)

p
#------------------------------------------------------------------------------------------------------------------------
# July 17th, lecture

## ML estimation of binomial & extended beta binomial y with predictors

## Number of "Yes" votes
## Bundesrat: Sept-Dec 1950


# model to print ----

model <- "binom"    # "ebbinom"


# variables and data ----

yvar <- "agree"
xvars <- c("cdu", "govparties")
trialsvar <- "votes"

path_data <- "~/Developement/RD2/votes-bundesrat.csv"


# extended beta binomial probability mass function ----
#notice that debbinom is not implimented in R, maybe there is a package but apparantly fuck it
# notice that the logarithm of the function has been implemented as log of this funcion behaves similarly and is easier to implement and calculate
debbinom <- function(y, size, rho, gamma, log = FALSE) {
  
  check_length <- length(size) == length(y) & length(rho) == length(y)
  stopifnot("y, size, and rho must have same length" = check_length)
  
  N <- size
  n <- length(y)
  
  lcombin <- lchoose(N, y) #the R function to get the log of binom
  
  lpr_elem <- sapply(
    seq_len(n),
    \(.i)
    sum(sapply(0:(y[.i] - 1), \(.j) log(rho[.i] + gamma * .j))) +
      sum(sapply(0:(N[.i] - y[.i] - 1), \(.j) log(1 - rho[.i] + gamma * .j))) -
      sum(sapply(0:(N[.i] - 1), \(.j) log(1 + gamma * .j)))
  )
  
  lpr <- lcombin + lpr_elem
  
  if (log)
    out <- lpr
  else 
    out <- exp(lpr)
  
  return(out)
  
}


# binomial log likelihood function ----

ll_binom <- function(theta, y, trials, x = NULL) {
  
  N <- trials
  beta <- theta
  
  if (is.null(x))
    xb <- beta
  else 
    xb <- cbind(1, x) %*% beta
  
  pr <- 1 / (1 + exp(-xb))
  
  ll <- sum(dbinom(y, size = N, prob = pr, log = TRUE))
  
  return(ll)
  
}


# extended beta binomial log likelihood function ----

ll_ebbinom <- function(theta, y, trials, x = NULL) {
  
  n_par <- length(theta)
  
  beta <- theta[-n_par]
  gamma <- theta[n_par]
  
  if (is.null(x))
    xb <- beta
  else
    xb <- cbind(1, x) %*% beta
  
  pr <- 1 / (1 + exp(-xb))
  
  ll <- sum(debbinom(y, size = trials, rho = pr, gamma = gamma, log = TRUE))
  
  return(ll)
  
}


# estimation inputs ----

data <- read.csv(path_data)
data
y <- data[, yvar]
x <- as.matrix(data[, xvars])
trials <- data[, trialsvar]

theta_start <- setNames(
  rep_len(0, ncol(x) + 1),
  nm = c("const", xvars)
)
theta_start

theta_start_add <- c("gamma" = 0)

theta_start <- switch(
  model,
  binom = theta_start,
  ebbinom = c(theta_start, theta_start_add)
)


# estimation ----

llf <- switch(model, binom = ll_binom, ebbinom = ll_ebbinom)

regres <- optim(
  fn = llf,
  par = theta_start,
  y = y,
  x = x,
  trials = trials,
  method = "BFGS",
  control = list(fnscale = -1),
  hessian = TRUE
)


# post estimation ----

# coefficient estimates ----

coefs <- regres$par

# standard errors ----

h <- regres$hessian
vcov <- -(solve(h))
se <- sqrt(diag(vcov))


# Wald test statistics for H_0: k = 0 ----

k <- 0

w <- (coefs - k) / se
p_val <- pnorm(abs(w), lower.tail = FALSE) * 2


# print result ----

round(
  data.frame(Coef = coefs, SE = se, z = w, p = p_val),
  digits = 3
)

switch(
  model,
  binom = cat("Binomial regression"),
  ebbinom = cat("Extended beta binomial regression")
)


