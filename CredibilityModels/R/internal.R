weighted.sigma <- function(x, w){
  1/length(x)*sum(w*(x-weighted.mean(x, w))^2)
}

