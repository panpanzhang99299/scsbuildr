##
## scsbuidr: A Presentation of building R Packages for the Statistical Computing Series

## Copyright (C) 2024 Panpan Zhang
## Panpan Zhang <panpan.zhang@vumc.org>
##
## This file is part of the R package scsbuildr.
##
## The R package scsbuildr is free software: You can redistribute it and/or
## modify it under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or any later
## version (at your option). See the GNU General Public License at
## <https://www.gnu.org/licenses/> for details.
##
## The R package scsbuildr is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
##

#' @importFrom mvtnorm dmvnorm
NULL

## log_mvtnorm: Compute the logarithm of the likelihood of a sample point drawn 
## from a multivariate normal distribution based on **mvtnorm** package. 

#' Log of Multivariate Normal Density (Based on Another Package)
#'
#' This function derives the logarithm of the likelihood of a sample point drawn
#' from a multivariate normal distribution based on \code{mvtnorm} package. 
#'
#' @param x is a vector of sample point or a matrix of sample points
#' @param mu is the mean vector of multivariate normal distribution
#' @param Sigma is the variance-covariance matrix of multivariate normal distribution 
#' @param loglik is a logistic argument that determines whether or not to take the
#' logarithm of the density: \code{TRUE} (default), taking the logarithm; \code{FALSE}, 
#' otherwise.
#'
#' @return a value (vector or scalar) of the (logarithm of) likelihood of the sample point(s) with the
#' given multivariate distribution. 
#'
#' @references Zhang, P. (2024). Paper name. *Journal name*. **volume**(number), 1--20.
#'
#' @keywords internal
#'
#' @export
#' 
#' @examples
#' set.seed(314159)
#' mydata <- cbind(c(1, 2), c(3, 4))
#' mymu <- c(0, 0)
#' mySigma <- diag(2)
#' log_mvtnorm(x = mydata, mu = mymu, Sigma = mySigma)
#' 


log_mvtnorm <- function(x, mu, Sigma, loglik = TRUE) {
  p <- length(mu)
  
  if (!is.null(ncol(x))) {
    res <- sapply(1:ncol(x), function(x.iter) {
      mvtnorm::dmvnorm(x[, x.iter],
                       mean = mu,
                       sigma = Sigma,
                       log = loglik)
    })
  } else {
    res <- mvtnorm::dmvnorm(x,
                            mean = mu,
                            sigma = Sigma,
                            log = loglik)
  }
  
  return(res)
  
}

