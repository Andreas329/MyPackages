#' unsplit_k reverses the effect of split_k
#'
#' @param x result from split_k
#' @export

unsplit_k <- function(x){
  data <- do.call(rbind, x$data)
  rep_key <- function(key, nrow) {
   res <- key[rep(1, nrow), , drop = F]
   rownames(res) <- NULL
   return(res)
  }
  grouping <- do.call(function(...) rbind(..., make.row.names = F),
                      Map(rep_key, x$key, sapply(x$data, nrow)))
  res <- list(data = data, grouping = grouping)
  return(res)
}
