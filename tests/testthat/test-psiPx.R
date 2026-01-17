test_that("Check model type is correct", {

  expect_error(psiPx(model_type = "cheese"))

})

testthat::test_that("Check Linear model", {

  result <- hydrafit::psiPx(model_type = "Linear")(A = 1, B = 1, px = 0.5, max_cond_at = 0)
  result2 <- hydrafit::psiPx(model_type = "Linear")(A = 1, B = 2, px = 0, max_cond_at = 0)
  expect_equal(result$psi.px, -0.5)
  expect_equal(result$max_c, 1)

  #what is the water potential and max cond at 0% loss in conductance?
  expect_equal(result2$psi.px, 0)
  expect_equal(result2$max_c, 2)
})

testthat::test_that("Check exponential models", {

  result <- hydrafit::psiPx(model_type = "exp")(A = 1, B = 1, px = 0, max_cond_at = 0)
  # if px equals zero expect that psi.px = max_cond_at
  expect_equal(result$psi.px, 0)
  expect_equal(result$max_c, 1)

  result2 <- hydrafit::psiPx(model_type = "exp")(A = 1, B = 1, px = 0.5, max_cond_at = 0)

  expect_equal(result2$psi.px, 0.69314718)
  expect_equal(result2$max_c, 1)

  result_exp2 <- hydrafit::psiPx(model_type = "exp2")(A = 1, B = 1, C = 0, px = 0, max_cond_at = 0)
  # if px equals zero expect that psi.px = max_cond_at
  expect_equal(result_exp2$psi.px, 0)
  expect_equal(result_exp2$max_c, 1)

})

testthat::test_that("Check sigmoidal and logistic models", {

  result <- hydrafit::psiPx(model_type = "sig")(A = 1, B = 1, C = 1, px = 0.5, max_cond_at = 0)
  # if px equals zero expect that psi.px = max_cond_at
  expect_equal(result$psi.px, -0.8619948)
  expect_equal(result$max_c, 0.268941421)

  result2 <- hydrafit::psiPx(model_type = "log")(A = 1, B = 1, C = 1, px = 0.5, max_cond_at = 0)

  expect_equal(result2$psi.px, 1)
  expect_equal(result2$max_c, 1)

  # p80?
  result2_p80 <- hydrafit::psiPx(model_type = "log")(A = 1, B = 1, C = 1, px = 0.8, max_cond_at = 0)

  expect_equal(result2_p80$psi.px, 4)
  expect_equal(result2_p80$max_c, 1)

})
