#BF method for rem class objects


#' @method BF rem
#' @export
BF.rem <- function(x,
                   hypothesis = NULL,
                   prior = NULL,
                   ...){

  #Extract summary statistics
  n <- x$df.null
  sigma <- x$cov
  if (!isTRUE(all.equal(sigma, t(sigma))) || any(diag(sigma) < 0)){
    stop("The rem object contained a non-positive definite covariance matrix. This problem appears to occur when running rem in a 32 bit environment. The results of that analysis may not be trustworthy. We have reached out to the maintainer of the relevent package. Please use a 64 bit environment for now.")
  }
  out <- BF(x$coef, hypothesis, prior, sigma=sigma, n=n)
  out$model <- x
  out$call <- match.call()
  out

}
