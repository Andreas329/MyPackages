context("split_k")
test_that("1 column",{
  df <- data.frame(A = 1:10)
  grouping <- data.frame(K = rep(c("a", "c"), each = 5))

  expect_equal(split_k(df, grouping),
               list(data = list(data.frame(A = 1:5), data.frame(A = 6:10)),
                    key = list(data.frame(K = factor("a", levels = c("a", "c"))),
                               data.frame(K = factor("c", levels = c("a", "c"))))))

})

test_that("two columns", {

  df <- data.frame(A = 1:10, B = 11:20)
  grouping <- data.frame(K = rep(c("a", "c"), each = 5), stringsAsFactors = F)

  expect_equal(split_k(df, grouping),
               list(data = list(data.frame(A = 1:5, B= 11:15), data.frame(A = 6:10, B = 16:20)),
                    key = list(data.frame(K = "a", stringsAsFactors = F),
                               data.frame(K = "c", stringsAsFactors = F))))

})

test_that ("two key columns", {
  df <- data.frame(A = 1:10, B = 11:20)
  grouping <- data.frame(K = rep(c("a", "c"), each = 5), K2= c(rep(1, 3), rep(2, 5), rep(3, 2)))
  expect_equal(split_k(df, grouping),
               list(data = list(data.frame(A = 1:3, B= 11:13), data.frame(A = 4:5, B = 14:15),
                                data.frame(A = 6:8, B = 16:18), data.frame(A = 9:10, B = 19:20)),
                    key = list(data.frame(K = factor("a", levels = c("a", "c")), K2 = 1),
                               data.frame(K = factor("a", levels = c("a", "c")), K2 = 2),
                               data.frame(K = factor("c", levels = c("a", "c")), K2= 2),
                               data.frame(K = factor("c", levels = c("a", "c")), K2= 3))))
})

