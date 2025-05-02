test_that("it works", {
  result1 <- myncurve(0, 1, 1)
  expect_equal(result1$area, pnorm(1, mean=0, sd=1),tolerance=1)



  result2 <- myncurve(6, 6, 6)
  expect_equal(result1$area, pnorm(6, mean=6, sd=6),tolerance=1)


  expect_error(myncurve("mu", 1, 1), "non-numeric argument")

}
)
