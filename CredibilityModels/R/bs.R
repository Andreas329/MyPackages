#' Fits Buehlmann-Straub-Model.
#'
#' @param formula a formula
#' @param data a data.frame in which to interpret the variables named in formula.
#' @param weights expression indicating the column of data containing the weights.
#' @return bs returns an object of class cm.
#' @export
#' @examples
#' bs(KS ~ Region, testdat, count)
bs <- function(formula, data, weights, ... ) {
  #create model.frame
  cl <- match.call()
  mf <- match.call(expand.dots = FALSE)
  m <- match(c("formula", "data", "weights"), names(mf), 0L)
  mf <- mf[c(1L, m)]
  mf[[1L]] <- quote(stats::model.frame)
  mf <- eval(mf, parent.frame())
  #todo: formula check

  #get relevant data
  y <- model.response(mf, "numeric")
  w <- model.weights(mf)
  group <- mf[, attr(attr(mf, "terms"), "term.labels"), drop = F]
  if(any(apply(group, 2, function(x) length(grep("\\.", x, value = F)) > 1))){
    stop("variables used for grouping cannot contain . (variables after ~ in formula)")
  }

  #calc estimators for sigma
  data.split <- split_k(data.frame(y = y, w = w), group)

  s.k <- sapply(data.split$data, function(data) weighted.sigma(data$y, data$w))
  s.est <- weighted.mean(s.k, sapply(data.split$data, nrow))

  #calc estimator for tau
  w.group <-  sapply(data.split$data, function(data) sum(data$w))
  y.group <-  sapply(data.split$data, function(data) weighted.mean(data$y, data$w))
  nominator <- sum(w.group*(y.group - weighted.mean(y, w))^2) - (length(data.split$data)-1) * s.est
  denominator <- sum(w) - sum(w.group^2)/sum(w)
  tau.est <- max(nominator/denominator, 0)

  #create output
  kappa <- ifelse(tau.est == 0 , 0, s.est/tau.est)
  alpha <- w.group / (w.group + kappa)
  mu <- weighted.mean(y.group, alpha)
  cred.est <- alpha * y.group + (1-alpha) * mu

  est <- unsplit_k(list(data =  lapply(alpha, function(x) data.frame(alpha = x, row.names = NULL)),
                 key = data.split$key))
  est <- data.frame(est$grouping, est$data, mu_ind = y.group, cred_est = cred.est)

  return(list(s.sq = s.est, tau = tau.est, kappa = kappa, mu = mu, est =  est))
}


