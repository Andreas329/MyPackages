#' Divides the data in a data.frame according to an other data.frame. Returns list with splited data.frame's and
#' corresponding key
#'
#' @param data a data.frame
#' @param grouping a data.frame where all columns will be used for grouping x
#' @param keep.row.names keep the row.names of data. Note that the rownames of grouping will always be lost.
#' @export

split_k <- function(data, grouping, keep.row.names = F){
  if(!is.data.frame(data) | !is.data.frame(grouping)){
    stop("data and grouping must be a data.frame")
  }
  if(nrow(data) != nrow(grouping)){
    stop("nrow(data) must be equal to nrow(grouping")
  }
  g.u <- split(unique(grouping), unique(grouping), drop = T)
  names(g.u) <- NULL
  g.u <- lapply(g.u, function(x) {rownames(x) <- NULL; return(x)})
  g.rows <- lapply(g.u, function(x) which(apply(grouping, 1, function(row) all(row == x))))
  split_data <- function(row.num){
      res <- data[row.num, , drop = F]
      if(!keep.row.names) {rownames(res) <- NULL}
      return(res)
  }
  data.split <- lapply(g.rows, split_data)
  res <- list(data = data.split, key = g.u)
  return(res)
}
