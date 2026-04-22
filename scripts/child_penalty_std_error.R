
estimates <- calc_estimates(overall_df_11)[[1]]
# t(c(1/2, 1/4)) %*% matrix(c(8, 4, 4, 8), nrow = 2) %*% c(1/3, 1/5)



weights_m <- 1/estimates$denominator.m
weights_f <- 1/estimates$denominator.f

is_post <- c(rep(FALSE, times = 5), rep(TRUE, times = 11))

weights_m <- ifelse(is_post, 1/11 * weights_m, -1/5 * weights_m)
weights_f <- ifelse(is_post, -1/11 * weights_f, 1/5 * weights_f)

w <- c(weights_m, weights_f)
beta_s <- c(estimates$coefs.m1, estimates$coefs.f1)

cov_m <- as.matrix(vcov(calc_estimates(overall_df_11)[[3]]))
cov_f <- as.matrix(vcov(calc_estimates(overall_df_11)[[2]]))

## Add 0 rows and columns in t = -2.

cov_m <- rbind(cov_m[1:3,] , `t-2`= rep(0, 15), cov_m[4:15,])
cov_m <- cbind(cov_m[, 1:3], `t-2` = rep(0, 16), cov_m[, 4:15])

cov_f <- rbind(cov_f[1:3,] , `t-2`= rep(0, 15), cov_f[4:15,])
cov_f <- cbind(cov_f[, 1:3], `t-2` = rep(0, 16), cov_f[, 4:15])

COV <- as.matrix(bdiag(cov_m, cov_f))

penalty <- t(w) %*% beta_s
variance <- t(w) %*% COV %*% w
cp_sd <- sqrt(variance)




vcov(b)[6:16, 6:16]

cov <- as.matrix(vcov(b))
cov <- rbind(cov[1:3,] , `t-2`= rep(0, 15), cov[4:15,])
cov <- cbind(cov[, 1:3], `t-2` = rep(0, 16), cov[, 4:15])

c<- 1/121*(t(weights_m[6:16]) %*% cov[6:16, 6:16] %*% weights_f[6:16])

d <- 1/25*(t(weights_m[1:5]) %*% cov[1:5, 1:5] %*% weights_f[1:5])


sum <- 0
for (i in 1:length(w)) {
  for (j in 1:length(w)){
    item <- w[i] * w[j] * cov[6:16, 6:16][i, j]
    sum <- sum + item
  }
}

# Smaller example.

w <- runif(6, 1, 1.5)

sample <- tibble(
  x1 = rnorm(100, 0, 0.5), 
  x2 = rnorm(100, 0.5 * x1, 2),
  x3 = rnorm(100, 1, 0.2)
)

c <- cov(sample)




## ----- Block Diagonal Matrix -----##

as.matrix(bdiag(
  matrix(c(1, 2, 3, 4), nrow = 2),
  matrix(c(5, 6, 7,8), nrow = 2)
 ))


# Long form penalty estimation.

with(estimates, 1/11 *sum((penalty.m[6:16] - penalty.f[6:16]))) - 
  with(estimates, 1/5 * sum(penalty.m[1:5] - penalty.f[1:5]))
