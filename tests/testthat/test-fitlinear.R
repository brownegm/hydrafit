# test that linear fits works
testthat::test_that("linear fit is correct", {

# create test values and data frame
set.seed(2)

A = 1
B = 2

psi <-rnorm(100,3,1)
species <- rep("test", length(psi))

kl <- hydrafit::Linear(A=A, B=B, psi = psi)

test_df <- data.frame(species = species, psi=psi, kl=kl)

# Run fit_linear function
suppressWarnings({
  linear_fits = hydrafit::fit_vuln_curve(input_df = test_df, model_type = "Linear", max_cond_at = 0.1)
})
print(linear_fits$A)
print(linear_fits$B)
# Test that the function predicts the correct values
Apred = round(linear_fits[["A"]], 1)
Bpred = round(linear_fits[["B"]], 1)

#print(Apred)
testthat::expect_equal(Apred, A)
testthat::expect_equal(Bpred, B)
})

