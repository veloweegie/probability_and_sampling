set.seed(130)

N <- 1000
n <- 9
y <- abs(rnorm(N))
y_items <- c(0:8)
#y_sample <- sample(y, n
#y_sample <- c(28,23,25,33,31,18,22,29,30,22,26,20,21,28,25)
y_sample <- c(33.5
              ,	32.0
              ,	52.0
              ,	43.0
              ,	40.0
              ,	41.0
              ,	45.0
              ,	42.5
              ,	39.0)

y_mean_hat <- sample_mean(n, y_sample)
cat('Sample mean, y_hat: ', y_mean_hat)
y_sq <- sum(y_sample^2)
cat('Y squared: ', y_sq)
y_var_hat <- mean_estimate_var(n, N, y_sq, y_mean_hat)
cat('Variance from sample mean: ', y_var_hat)

# Calculate CI for sample mean
ci <- ci_mean(95, n, N, y_var_hat)
paste0(
  round(y_mean_hat - ci, 4), 
  ' < ', y_mean_hat, ' < ',
  round(y_mean_hat + ci, 4), 
  ' variance: ', y_var_hat
)

y_total_hat <- total_estimate(n, N, y_sample)
cat('Estimate of total: ', y_total_hat)
y_total_var_hat <- total_estimate_var(n, N, y_sq, y_mean_hat)
cat('Variance estimator of sample mean (N^2 * sample variance): ', y_total_var_hat)


# Calculate CI for sample total
ci <- ci_total(95, n, N, y_total_var_hat)
paste0(
  round(y_total_hat - ci, 4), 
  ' < ', y_total_hat, ' < ',
  round(y_total_hat + ci, 4), 
  ' variance: ', y_total_var_hat
)

# calculate sample size needed for required error margin and CI
N <- 25
sample_var <- 136.3
d <- 5
level <- 95
n <- sample_size(N, sample_var, d, level)
paste0('Sample size must be >= to ', n)


# Helper functions*
t_score <- function(alpha, n) {
  cat('T-score - sample less than or equal to 40')
  return(qt(1 - (alpha/2), n-1))
}

z_score <- function(alpha) {
  cat('Z-score - sample greater than 40')
  return(qnorm(1 - (alpha/2)))
}

sample_mean <- function(n, y) {
  return((1/n) * sum(y))
}

f <- function(n, N) {
  return(n/N)
}

sample_mean_var <- function(n, N, estimator_sq, sample_mean) {
  var <- (1/(n-1)) * (estimator_sq - (n * sample_mean^2))
  cat('Sample variance: ', var)
  return(var)
}

total_estimate <- function(n, N, y) {
  return((N/n) * sum(y))
}

mean_estimate_var <- function(n, N, estimator_sq, sample_mean) {
  return((1-f(n, N)/n) * sample_mean_var(n, N, estimator_sq, sample_mean))
}

total_estimate_var <- function(n, N, estimator_sq, sample_mean) {
  return((N^2) * ((1-f(n, N))/n) * sample_mean_var(n, N, estimator_sq, sample_mean))
}

ci_mean <- function(level, n, N, sample_mean_var) {
  alpha <- (100-level) / 100
  score <- ifelse(n > 40, z_score(alpha), t_score(alpha, n))
  return(score * sqrt(sample_mean_var))
}

ci_total <- function(level, n, N, sample_mean_var) {
  alpha <- (100-level) / 1004
  score <- ifelse(n > 40, z_score(alpha), t_score(alpha, n))
  return(score * sqrt(sample_mean_var))
}

sample_size <- function(N, sample_mean_var, d, level) {
  alpha <- (100-level) / 100
  return((N * sample_mean_var) 
         / (sample_mean_var + (N * (d / z_score(alpha))^2)))
}