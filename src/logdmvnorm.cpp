#include <RcppArmadillo.h>
#include <math.h>
using namespace Rcpp;
using namespace arma;

// [[Rcpp::depends(RcppArmadillo)]]
//' Derive the logarithm of the likelihood of a sample point from a multivariate normal distribution
//' 
//' @param X Vector, a vector of numeric values
//' @param mean Vector, the mean of multivariate normal distribution
//' @param Sigma Matrix, the variance-covariance matrix of multivariate normal distribution 
//' @return Returns the logarithm of the likelihood of X from the multivariate normal distribution 
//' with mean \code{mean} and variance \code{Sigma}
//'
//' @keywords internal
//'
// [[Rcpp::export]]
double logdmvnorm (arma::rowvec const &X, // Input
                   arma::rowvec const &mean, // Mean vector
                   arma::mat const &Sigma // Variance-Covariance Matrix
){
  
  int p = X.n_cols;
  
  arma::mat const rootSigmainv = arma::inv(arma::trimatu(arma::chol(Sigma)));
  
  double const Sigmaconst = arma::sum(log(rootSigmainv.diag()));
  
  double const piconst = -(double)p/2 * std::log(2*M_PI);
  
  arma::rowvec z = (X - mean) * rootSigmainv;
  
  double out = Sigmaconst + piconst - 0.5 * arma::dot(z, z);
  
  return out;
                          
}