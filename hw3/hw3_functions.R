generate_data <- function(n = 500){
  dat <- MASS::mvrnorm(n, mu = c(0,0), Sigma = matrix(c(10,-2,-2,1), 2, 2))
  scale(dat, center = T, scale = T)
}