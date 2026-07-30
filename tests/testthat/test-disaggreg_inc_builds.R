test_that("disaggregation build_A function works", {
  obs_times <- seq(8, 100, by = 7)
  N <- max(obs_times)
  window <- 7

  A <- build_A(N, obs_times, window)
  d <- A@Dim

  expect_true(class(A) == "dgCMatrix")
  expect_true(d[1] == length(obs_times))
  expect_true(d[2] == N)
  expect_all_true(rowSums(as.matrix(A)) == window)
})

test_that("disaggregation build_D2 function works", {
  N <- 20
  D <- build_D2(N)
  d <- D@Dim
  expect_equal(d[2], N)
  expect_all_true(rowSums(as.matrix(D)) == 0)
})
