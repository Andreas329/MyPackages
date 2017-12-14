#' Fits Buehlmann-Straub-Model.
#'
#' @param formula a formula
#' @param data a data.frame in which to interpret the variables named in formula.
#' @param weights expression indicating the column of data containing the weights.
#' @return bs returns an object of class cm.
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

  #calc estimators for sigma
  data.split <- split(data.frame(y = y, w = w), group)

  s.k <- sapply(data.split, function(data) weighted.sigma(data$y, data$w))
  s.est <- weighted.mean(s.k, sapply(data.split, nrow))

  #calc estimator for tau
  w.group <-  sapply(data.split, function(data) sum(data$w))
  y.group <-  sapply(data.split, function(data) weighted.mean(data$y, data$w))
  nominator <- sum(w.group *(y.group - weighted.mean(y, w))^2) - length(data.split) * s.est
  denominator <- sum(w) - sum(w.group^2)/sum(w)
  tau.est <- max(nominator/denominator, 0)

  #create output
  kappa <- s.est/tau.est
  alpha <- w.group / (w.group + kappa)
  cred.est <- alpha * weighted.mean(y, w) + (1-alpha) *y.group
  return(list(s.est = s.est, tau.est = tau.est, kappa = kappa, alpha = alpha, cred.est = cred.est))
}


