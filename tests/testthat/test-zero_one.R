context("zero_one")

test_that("scale_zero_one() works", {
  vec = rnorm(100)

  int = sample(100, size = 20)

  neg = sample(-100:-1, size = 20)

  expect_equal(range(scale_zero_one(vec)), c(0, 1))
  expect_equal(range(scale_zero_one(int)), c(0, 1))
  expect_equal(range(scale_zero_one(neg)), c(0, 1))
})
