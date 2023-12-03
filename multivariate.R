# Linear multivariate transformation, AX + b
# of multivariate normal distribution, creating a new
# multivariate normal distribution

# Constants A and b to use for transformation
A <- rbind(
      c(1,-1)
    )
b <- c(-0.5,-0.5)

# Mean vector
mean <- c(2.5,2)

# Covariance Matrix
covar <- rbind(
    c(1,1),
    c(1,2)
  )

# Do the transformation
trans_mean <- A %*% mean + b
trans_covar <- A %*% covar %*% t(A)

# Output result
trans_mean
trans_covar

# Conditional expectation
mu1 <- 2
mu2 <- -3
sigma1 <- 4
sigma2 <- 16
covar <- -7.2
corr <- -0.9
x <- -0.9

mu1x <- mu1 + ((sqrt(sigma1)/sqrt(sigma2))*corr*(x-mu2))
mu2x <- mu2 + ((sqrt(sigma2)/sqrt(sigma1))*corr*(x-mu1))
sigma1x <- (1-(corr^2)) * sigma1
sigma2x <- (1-(corr^2)) * sigma2
mu1x
sigma1x
mu2x
sigma2x
