test_that("Check model type is correct", {

  expect_error(psiPx(model_type = "cheese"))

})

testthat::test_that("Check Linear model", {

  result <- hydrafit::psiPx(model_type = "Linear")(A = 2, B = 3, px = 0.4, max_cond_at = 0)
  result2 <- hydrafit::psiPx(model_type = "Linear")(A = 2, B = 3, px = 0, max_cond_at = 0)
  expect_equal(result$psi.px, -0.6)
  expect_equal(result$max_c, 3)

  #what is the water potential and max cond at 0% loss in conductance?
  expect_equal(result2$psi.px, 0)
  expect_equal(result2$max_c, 3)

})
