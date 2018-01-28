context("unsplit_k")
test_that("1 column",{
  df <- data.frame(A = 1:10)
  grouping <- data.frame(K = rep(c("a", "c"), each = 5))

  expect_equal(unsplit_k(split_k(df, grouping)),
               list(data = df, grouping = grouping))

})

test_that("two columns", {
  df <- data.frame(A = 1:10, B = 11:20)
  grouping <- data.frame(K = rep(c("a", "c"), each = 5), stringsAsFactors = F)

  expect_equal(unsplit_k(split_k(df, grouping)),
               list(data = df, grouping = grouping))

})

test_that ("two key columns", {
  df <- data.frame(A = 1:10, B = 11:20)
  grouping <- data.frame(K = rep(c("a", "c"), each = 5), K2= c(rep(1, 3), rep(2, 5), rep(3, 2)))
  expect_equal(unsplit_k(split_k(df, grouping)),
               list(data = df, grouping = grouping))
})

test_that ("two key columns with rownames", {
  df <- data.frame(A = 1:10, B = 11:20)
  rownames(df) <- paste0("A_", 1:10)
  grouping <- data.frame(K = rep(c("a", "c"), each = 5), K2= c(rep(1, 3), rep(2, 5), rep(3, 2)))
  expect_equal(unsplit_k(split_k(df, grouping, keep.row.names = T)),
               list(data = df, grouping = grouping))
})



