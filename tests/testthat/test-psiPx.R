test_that("Check model type is correct", {

  expect_error(psiPx(model_type = "cheese"))

})

testthat::test_that("Check Linear model", {

  result <- hydrafit::psiPx(model_type = "Linear")(A = 2, B = 3, px = 0.4, max_cond_at = 0)

  expect_equal(result$psi.px, -0.6)
  expect_equal(result$max_c, 3)

})
