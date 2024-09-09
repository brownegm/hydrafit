# test_that("multiplication works", {
#   expect_equal(2 * 2, 4)
# })

A = 15
B = -2
test_psi <-  rnorm(100, 3, 1)

linear <- Linear(A=A, B=B, test_psi)

plot(linear~test_psi)

# declare the data frame to store the results
modelfitting_results_linear <-
  data.frame(
    Species = as.character(),
    data.type = as.character(),
    A = numeric(),
    B = numeric(),
    C = numeric(),
    D = numeric(),
    loglikeli = numeric(),
    rsq = numeric(),
    slope = numeric(),
    AIC = numeric(),
    AICcorr = numeric(),
    sterror1 = numeric(),
    sterror2 = numeric(),
    sterror3 = numeric(),
    sterror4 = numeric(),
    N = numeric()
  )

#run all on the data linear should be the bestfit

