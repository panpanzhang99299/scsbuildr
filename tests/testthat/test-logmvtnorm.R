test_that("log of multivariate normal",
          {
            my_equal_test <- function(x, mu, Sigma, logmvtnorm_true) {
              logmvtnorm_val1 <- loglik_mnorm(x, mu, Sigma, loglik = TRUE)
              logmvtnorm_val2 <- log_mvtnorm(x, mu, Sigma, loglik = TRUE)
              logmvtnorm_val3 <- log_mnormcpp(x, mu, Sigma, loglik = TRUE)
              expect_equal(round(logmvtnorm_val1, 3), round(logmvtnorm_true, 3))
              expect_equal(round(logmvtnorm_val2, 3), round(logmvtnorm_true, 3))
              expect_equal(round(logmvtnorm_val3, 3), round(logmvtnorm_true, 3))
            }
            
            my_equal_test(
              x = c(3, 4),
              mu = c(2, 1),
              Sigma = diag(2),
              logmvtnorm_true = -6.837877
            )
          })