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
test_df_exp2 <- data.frame(psi=psi, k = kl_exp2)
test_df_sig <- data.frame(psi=psi, kl=kl_sig)
test_df_log <- data.frame(psi=psi, kl=kl_log)

#cebe_test <- hydrafit:::scof2012|>dplyr::filter(species=="cebe")
# check that the max_cond_at error is working (it should be >0)
 expect_error(hydrafit::fit_vuln_curve(kl~psi,input_df = test_df_exp, model_type = "exp", max_cond_at = 0))
 # check that the model type error is working (it should be one of the options above not test)
 expect_error(hydrafit::fit_vuln_curve(kl~psi, input_df = test_df_exp2, model_type = "test"))

 # check that changing the column names doesn't break the fx
expect_no_error(hydrafit::fit_vuln_curve(k~psi, input_df = test_df_exp2, model_type = "exp", max_cond_at = 0.1))
}
)

# I could test for the closest fitting value to the Kmax that I know?
# Should I just be testing the function itself or all together?
#
## Kmax values from Scoffoni et al. 2012
#cebe4.37, heca5.74, quag3.96, laca11.4, hear20.7
