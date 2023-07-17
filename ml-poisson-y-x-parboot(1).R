## ML estimation of Poisson y with predictors

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


path_data <- "~/Developement/RD2/assoc-memberships.csv"

# log-likelihood function for Poisson regression ----

ll_pois <- function(theta, y, x = NULL) {
  
  beta <- theta
  
  if (is.null(x))
    xb <- beta
  else 
    xb <- cbind(1, x) %*% beta
  
  lamb <- exp(xb)
  
  ll <- sum(dpois(y, lambda = lamb, log = TRUE))
  
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
  fn = ll_pois,
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
coefs_boot <- MASS::mvrnorm(n = n_boot, mu = coefs, Sigma = vcov)


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
x_obs <- data[, xvars]
x_obs

lamb<- exp(as.matrix(cbind(1, x_obs))%*% coefs)
pr<-sapply(outcome, dpois, lambda=lamb)
pr_avg <- colMeans(pr)
pr

round(pr_avg, digits=3) #predictions of our model
round(prop.table(table(y)), digits = 3)

plot(
  outcome,
  pr_avg,
  type="h",
  main = "Model prediction",
  ylim = c(0, 0.5)
)

table
