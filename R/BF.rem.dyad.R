#BF method for rem.dyad class objects


#' @method BF rem.dyad
#' @export
BF.rem.dyad <- function(x,
                        hypothesis = NULL,
                        prior = NULL,
                        ...){

  # if(.Machine$sizeof.pointer == 4){
  #   stop("BF currently does not evaluate rem.dyad objects computed in a 32 bit environment, because we have observed some unexpected results in the output (non-positive definite covariance matrices). We have reached out to the maintainer of the relevent package. Please use a 64 bit environment for now.")
  # }
  #Extract summary statistics
  cl <- match.call()
  get_est <- get_estimates(x)
  if (!isTRUE(all.equal(get_est$Sigma[[1]], t(get_est$Sigma[[1]]))) || any(diag(get_est$Sigma[[1]]) < 0)){
    stop("The rem.dyad object contained a non-positive definite covariance matrix. This problem appears to occur when running rem.dyad in a 32 bit environment. The results of that analysis may not be trustworthy. We have reached out to the maintainer of the relevent package. Please use a 64 bit environment for now.")
  }
  cl[[1]] <- as.name("BF")
  cl[["x"]] <- get_est$estimate
  cl[["sigma"]] <- get_est$Sigma[[1]]
  cl[["n"]] <- x$m
  out <- eval.parent(cl)
  out$model <- x
  out$call <- match.call()
  out

}


