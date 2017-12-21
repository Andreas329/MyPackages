weighted.sigma <- function(x, w){
  1/(length(x)-1)*sum(w*(x-weighted.mean(x, w))^2)
}

