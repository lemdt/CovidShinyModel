context("Helpers")

# getBetaFromDoubling ----------------------------------------------------------

test_that("getBetaFromDoubling returns accurate results", {
  b1 <- getBetaFromDoubling(doubling.time = 1, gamma = 1)
  b2 <- getBetaFromDoubling(doubling.time = 4, gamma = 0.5)
  expect_equal(b1, 2)
  expect_equal(b2, 0.689207115002721)
})

test_that("getBetaFromDoubling can take multiple doubling times", {
  betas <- getBetaFromDoubling(c(1, 1), 1)
  expect_equal(betas, c(2, 2))
})

test_that("getBetaFromDoubling can take multiple gammas", {
  betas <- getBetaFromDoubling(1, c(1, 2))
  expect_equal(betas, c(2, 3))
})

test_that("getBetaFromDoubling returns NA if either input is NA", {
  b1 <- getBetaFromDoubling(1, NA)
  b2 <- getBetaFromDoubling(NA, 1)
  b3 <- getBetaFromDoubling(NA, NA)
  expect_true(is.na(b1))
  expect_true(is.na(b2))
  expect_true(is.na(b3))
})

test_that("getBetaFromDoubling gives meaningful result if doubling.time <= 0", {
  b1 <- getBetaFromDoubling(0, 1)
  b2 <- getBetaFromDoubling(-1, 1)
  expect_equal(b1, Inf)
  expect_equal(b2, 0.5)
})
