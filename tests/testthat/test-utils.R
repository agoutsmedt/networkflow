test_that("mixcolor() averages colours and transparency", {
  expect_equal(mixcolor("#000000", "#FFFFFF"), grDevices::rgb(127.5, 127.5, 127.5, 255, maxColorValue = 255))
  expect_equal(mixcolor(c("red", "blue"), c("red", "blue")), c("#FF0000FF", "#0000FFFF"))
  expect_equal(mixcolor("#FF0000", "#0000FF", amount1 = 1), "#FF0000FF")
})

test_that("mixcolor() gives the same result as the former row-wise version", {
  former <- function(col1, col2, amount1 = 0.5) {
    .mix <- function(col1, col2, amount1 = 0.5) {
      mix <- apply(grDevices::col2rgb(c(col1, col2), alpha = TRUE), 1,
                   function(x) amount1 * x[1] + (1 - amount1) * x[2])
      do.call("rgb", c(as.list(mix), maxColorValue = 255))
    }
    m <- suppressWarnings(cbind(col1, col2, amount1))
    apply(m, 1, function(x) .mix(col1 = x[1], col2 = x[2], amount1 = as.numeric(x[3])))
  }
  col1 <- c("#E69F00", "#56B4E9", "#009E73", NA)
  col2 <- c("#F0E442", "#56B4E9", "#D55E00", "#0072B2")
  expect_equal(mixcolor(col1, col2), former(col1, col2))
})

test_that("rank_by_size() numbers clusters by decreasing size", {
  expect_equal(rank_by_size(c(5, 5, 9, 9, 9, 2)), c(2, 2, 1, 1, 1, 3))
})

test_that("is_single_graph() tells a graph from a list of graphs", {
  expect_true(is_single_graph(toy_graph()))
  expect_false(is_single_graph(list(toy_graph(), toy_graph())))
  expect_error(is_single_graph(list(toy_graph(), 1)), "tbl_graph")
  expect_error(is_single_graph(list()), "tbl_graph")
})

test_that("check_explicit_choice() lists the possible values", {
  expect_equal(check_explicit_choice("a", c("a", "b"), "x"), "a")
  expect_error(check_explicit_choice(c("a", "b"), c("a", "b"), "x"), "Possible values")
})
