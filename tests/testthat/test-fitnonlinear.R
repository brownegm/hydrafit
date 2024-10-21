# test that linear fits works
testthat::test_that("Nonlinear fitted parameters are correct", {
# create test values and data frame
set.seed(2)

A = 1
B = 2
C = 0.05

psi <-  rnorm(100,3,1)

kl_exp <- hydrafit::Exponential(A = A, B = B, psi = psi)
kl_exp2 <- hydrafit::Exponential2(A = A, B = B, C = C, psi = psi)
kl_sig <- hydrafit::Sigmoidal(A = A, B = B, Xo = C, psi = psi)
kl_log <- hydrafit::Logistic(A = A, B = B, Xo = C, psi = psi)

test_df_exp <- data.frame(psi=psi, kl=kl_exp)
test_df_exp2 <- data.frame(psi=psi, kl= kl_exp2)
test_df_sig <- data.frame(psi=psi, kl=kl_sig)
test_df_log <- data.frame(psi=psi, kl=kl_log)

cebe_test <- hydrafit::scof2012|>dplyr::filter(species=="cebe")
# Run fit_linear function
suppressWarnings(### TRY USING "TRY()" HERE
try({
  exp_fits = hydrafit::fit_nonlinear(input_df = cebe_test, model_type = "exp")
  exp2_fits = hydrafit::fit_nonlinear(input_df = cebe_test, model_type = "exp2")
  sig_fits = hydrafit::fit_nonlinear(input_df = cebe_test, model_type = "sig")
  log_fits = hydrafit::fit_nonlinear(input_df = cebe_test, model_type = "log")
}, silent = TRUE
  )
)


# Test that the function predicts the correct values
# Apred = round(exp_fits[["A"]], 1)
# Bpred = round(exp_fits[["B"]], 1)
# #exponential 2
# Apred_exp2 = round(exp2_fits[["A"]], 1)
# Bpred_exp2 = round(exp2_fits[["B"]], 1)
# Cpred_exp2 = round(exp2_fits[["C"]], 1)
# # sigmoidal
# Apred_sig = round(exp_fits[["A"]], 1)
# Bpred_sig = round(exp_fits[["B"]], 1)
# Cpred_sig = round(exp_fits[["C"]], 1)
# # logistic
# Apred_logistic = round(exp_fits[["A"]], 1)
# Bpred_logistic = round(exp_fits[["B"]], 1)
# Cpred_logistic = round(exp_fits[["C"]], 1)

hydrafit::fx_select(NULL,log_fits, sig_fits, exp_fits, exp2_fits)

# test that the modelled values are equalled to the values I put in
expect_equal(Apred, A)
expect_equal(Bpred, B)
#exp2
expect_equal(Apred, A)
expect_equal(Bpred, B)
expect_equal(Cpred, C)
#sigmoidal
expect_equal(Apred, A)
expect_equal(Bpred, B)
expect_equal(Cpred, C)
#logistic
expect_equal(Apred, A)
expect_equal(Bpred, B)
expect_equal(Cpred, C)
})

# I could test for the closest fitting value to the Kmax that I know?
# Should I just be testing the function itself or all together?
#
## Kmax values from Scoffoni et al. 2012
#cebe4.37, heca5.74, quag3.96, laca11.4, hear20.7
