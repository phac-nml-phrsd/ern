test_that("dist_bulk_example works for the viruses implemented.", {
  
  virus = c('COVID', 'FLUA', 'FLUB', 'RSV')

  x = lapply(virus, dist_bulk_example)
    
  l = sapply(x, length)
  testthat::expect_true(all(l==3))
  
  n = sapply(x, names)
  for(j in seq_along(virus)){
    testthat::expect_true(any(n[,j] == 'gi'))
    testthat::expect_true(any(n[,j] == 'incub'))
    testthat::expect_true(any(n[,j] == 'fec'))
  }
})


test_that("dist_bulk_example fails for an unknown virus.", {
  testthat::expect_error(dist_bulk_example('foo'))
})


test_that("dist_bulk_example_reporting works for the jurisdiction implemented.", {
  
  jur = c('Canada')
  
  x = lapply(jur, dist_bulk_example_reporting)
  
  l = sapply(x, length)
  testthat::expect_equal(l, 2)
  
  n = sapply(x, names)
  for(j in seq_along(jur)){
    testthat::expect_true(any(n[,j] == 'repdelay'))
    testthat::expect_true(any(n[,j] == 'repfrac'))
  }
})


test_that("dist_bulk_example_reporting fails for an unknown jurisdiction.", {
  testthat::expect_error(dist_bulk_example_reporting('foo'))
})

