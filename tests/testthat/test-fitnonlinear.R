# test that linear fits works
testthat::test_that("Nonlinear fitted parameters are correct", {
# create test values and data frame
set.seed(2)

A = 1
B = 2
C = 0.005

psi <-  rnorm(100,3,1)

kl_exp <- hydrafit::Exponential(A=A, B=B, psi = psi)
kl_exp2 <- hydrafit::Exponential2(A=A, B=B, C=C, psi = psi)
kl_sig <- hydrafit::Sigmoidal(A=A, B=B, Xo = C, psi = psi)
kl_log <- hydrafit::Logistic(A=A, B=B, Xo = C, psi = psi)

test_df_exp <- data.frame(psi=psi, kl=kl_exp)
test_df_exp2 <- data.frame(psi=psi, kl= kl_exp2)
test_df_sig <- data.frame(psi=psi, kl=kl_sig)
test_df_log <- data.frame(psi=psi, kl=kl_log)

# Run fit_linear function
suppressWarnings({
  exp_fits = fit_nonlinear(input_df = test_df_exp, model_type = "exp")
  exp2_fits = fit_nonlinear(input_df = test_df_exp2, model_type = "exp2")
  sig_fits = fit_nonlinear(input_df = test_df_sig, model_type = "sig")
  log_fits = fit_nonlinear(input_df = test_df_log, model_type = "log")
})
# Test that the function predicts the correct values
Apred = round(linear_fits[["A"]], 1)
Bpred = round(linear_fits[["B"]], 1)

print(Apred)
expect_equal(Apred, A)
expect_equal(Bpred, B)
})
