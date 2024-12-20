# test that linear fits works
testthat::test_that("linear fit is correct", {
# create test values and data frame
set.seed(2)

A = 1
B = 2

psi <-  rnorm(100,3,1)

kl <- hydrafit::Linear(A=A, B=B, psi = psi)

test_df <- data.frame(psi=psi, kl=kl)

# Run fit_linear function
suppressWarnings({
  linear_fits = fit_linear(input_df = test_df)
})
# Test that the function predicts the correct values
Apred = round(linear_fits[["A"]], 1)
Bpred = round(linear_fits[["B"]], 1)

print(Apred)
expect_equal(Apred, A)
expect_equal(Bpred, B)
})
