## Max C should equal the value from the fit when "max_cond_at"=0
testthat::test_that("Psi Px predicts correct values", {

# Linear
 psi.px <- psiPx(fx_type = "Linear")

 A <- 2
 B <- 0.5

psi_at_p50_linear <- psi.px(A=A, B=B, px = 0.5, max_cond_at = 0)

expect_equal(psi_at_p50_linear$max_c, A)

# Something a little harder
# Sigmoidal

psi.px.sig <- psiPx(fx_type = "sig")

A <- 2
B <- 0.5
C <- 0.001

expected <- A / (1 + exp(-(- C/B)))
psi_at_p50_sigmoidal <- psi.px.sig(A=A, B=B, C=C, px = 0.5, max_cond_at = 0)

expect_equal(psi_at_p50_sigmoidal$max_c, expected)
})
