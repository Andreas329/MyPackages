##--------------------
## hierarch cred
##---------------------


X <- data.frame(unit = c("A", "B", "A", "B", "B"), hachemeister)
fit <- cm(~unit + unit:state, X, ratio.1:ratio.12, weight.1:weight.12)
predict(fit)
predict(fit, levels = "unit")	# unit credibility premiums only
summary(fit)
summary(fit, levels = "unit")

#
# Call:
#   cm(formula = ~unit + unit:state, data = X, ratios = ratio.1:ratio.12,
#      weights = weight.1:weight.12)
#
# Structure Parameters Estimators
#
# Collective premium: 1742.22
#
# Between unit variance: 87263.7
# Within unit/Between state variance: 13414.84
# Within state variance: 139120026
#
# Detailed premiums
#
# Level: unit
# unit Indiv. mean Weight   Cred. factor Cred. premium
# A    1962.45     1.475955 0.9056702    1941.675
# B    1524.94     1.720129 0.9179619    1542.765
#
# Level: state
# unit state Indiv. mean Weight Cred. factor Cred. premium
# A    1     2060.921    100155 0.9061701    2049.733
# B    2     1511.224     19895 0.6573469    1522.032
# A    3     1805.843     13735 0.5697845    1864.280
# B    4     1352.976      4152 0.2858991    1488.504
# B    5     1599.829     36110 0.7768832    1587.097


dat.fire <- data.frame(Rk1 = rep(c("A", "B", "A", "B", "B"), each = 5),
                       Rk2 =  c(rep(paste0("R", (1:5)), each = 5)),
                       Jahr = rep(1:5, 5),
                       Anzahl = c(729, 786, 872, 951, 1019,
                                  1631, 1802, 2090, 2300, 2368,
                                  796, 827, 874, 917, 944,
                                  3152, 3454, 3715, 3859, 4198,
                                  400, 420, 422, 424, 440),
                       NL = c(0.8, 1.4, 0.3, 0.88, 1.6,
                              0.06, 0.72, 0.16, 0.20, 0.38,
                              1.80, 0.6, 0.8, 1.9, 1.1,
                              0.56, 1.2, 0.84, 1.07, 0.8,
                              0.1, 0, 0.4, 2.4, 0.1))

library(tidyr)

dat.fire.cm <-
  cbind(spread(dat.fire[, c("Rk1", "Rk2", "Jahr", "NL")], value = NL, key = Jahr),
      spread(dat.fire[, c("Rk1", "Rk2", "Jahr", "Anzahl")], value = Anzahl, key = Jahr))
dat.fire.cm <- dat.fire.cm[, -(8:9)]
colnames(dat.fire.cm)[3:7] <- paste0("NL", 1:5)
colnames(dat.fire.cm)[8:12] <- paste0("Anzahl", 1:5 )
fit <- cm(~Rk1 + Rk1:Rk2, dat.fire.cm, NL1:NL5, Anzahl1:Anzahl5)

#
# Structure Parameters Estimators
#
# Collective premium: 0.8469465
#
# Between Rk1 variance: 0.0838834
# Within Rk1/Between Rk2 variance: 0.05631335
# Within Rk2 variance: 261.1139

hc(formula = NL ~ Rk1 + Rk2, weights = Anzahl, data = dat.fire)


hc <- function(formula, data, weights, ... ) {
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

  browser()

  data.split <- split_k(data.frame(y = y, w = w), group)
  #calc estimators for sigma
  s.k <- sapply(data.split$data, function(data) weighted.sigma(data$y, data$w))
  s.est <- weighted.mean(s.k, sapply(data.split$data, nrow))
  #calc estimator for tau
  w.y.group <-  do.call(rbind,lapply(data.split$data, function(data)
    data.frame(w = sum(data$w), y = weighted.mean(data$y, data$w))))
  nominator <- sum(w.y.group$w*(w.y.group$y - weighted.mean(y, w))^2) -
    (length(data.split$key)-1) * s.est
  denominator <- sum(w) - sum(w.y.group$w^2/sum(w))
  tau.est <- max(nominator/denominator, 0)

  #create output
  kappa <- ifelse(tau.est == 0 , 0, s.est/tau.est)
  alpha <- w.y.group$w / (w.y.group$w + kappa)
  mu <- weighted.mean(w.y.group$y, alpha)
  cred.est <- alpha * w.y.group$y + (1-alpha) * mu

  alpha.g <- unsplit_k(list(data =  lapply(alpha, function(x) data.frame(alpha = x, row.names = NULL)),
                            key = data.split$key))
  return(list(s.sq = s.est, tau = tau.est, kappa = kappa, mu = mu,
              est = cbind(alpha.g$grouping, alpha.g$data, w.y.group, est = cred.est)))
}


