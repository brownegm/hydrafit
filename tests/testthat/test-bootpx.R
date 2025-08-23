test_that("bootstrap check", {

data <- scof2012

# outputs for the fits
linear_fits <- list()
logistic_fits <- list()
sigmoidal_fits <- list()
exp1_fits <- list()
exp2_fits <- list()

formula <- kl~psi
# fit the models
for (ii in seq_along(unique(data$species))){
  subset(data, data$species == unique(data$species)[ii], select = c(1:ncol(data))) -> data_by_sp

  linear_fits[[ii]] = hydrafit::fit_vuln_curve(formula, input_df = data_by_sp, model_type = "Linear", max_cond_at = 0.1, plot = F)

  logistic_fits[[ii]] = fit_vuln_curve(formula,data_by_sp, model_type = "log", plot=F)

  sigmoidal_fits[[ii]] = fit_vuln_curve(formula,data_by_sp, model_type= "sig", plot=F)

  exp1_fits[[ii]] = fit_vuln_curve(formula,data_by_sp, model_type = "exp", plot=F)

  exp2_fits[[ii]] = fit_vuln_curve(formula,data_by_sp, model_type= "exp2", plot=F)
 }

# run resample
best_model <- fx_select(linear_fits, logistic_fits, sigmoidal_fits, exp1_fits, exp2_fits)
testthat::expect_equal(attr(best_model, "fit.list"), TRUE)

# set parameters for the resamples
fit <- best_model[[1]]
testthat::expect_equal(attr(fit, "fit.list"), FALSE)

# run bootstrap for single fit and list of fits
psi_max = 0.1
px = 0.5
sims = 1000

bootstrap <- bootPX(fit, psi_max=0.1, seed = 202, margin = "quantile")

bootstrap_list <- bootPX(best_model, psi_max=0.1, seed = 202, margin = "quantile")

bootstrap_listc <- bootPX(best_model, psi_max=0.1, seed= 202, pairwise = T)
# check if the bootstrap results are the expected size
testthat::expect_equal(length(bootstrap), 10)
testthat::expect_equal(length(bootstrap_list), length(unique(data$species)))

# check if the bootstrap results are the same
testthat::expect_equal(bootstrap$margin_error, bootstrap_list[[1]]$margin_error)

updated_bootstrap_list <- bootstrap_list

# check if the bootstrap results are the same when one is changed
updated_bootstrap_list[[3]] <- exp2_fits[[3]]

#check the change
testthat::expect_equal(updated_bootstrap_list[[3]]$margin_error, exp2_fits[[3]]$margin_error)

# check the others are the same
upd_nonchanged <- updated_bootstrap_list[c(1,2,4,5)]
bootlist_nonchanged <- bootstrap_list[c(1,2,4,5)]

testthat::expect_identical(upd_nonchanged,bootlist_nonchanged)

# try again to make sure you still get the same answer
bootstrap2.0<-bootPX(fit, psi_max=0.1, seed = 202)

testthat::expect_identical(bootstrap,bootstrap2.0)
})


test_that("Psi_max is provided to the resample function", {

  expect_error(resamplePX(c("this", "is", "a", "fake", "fit"), seed=123, psi_max = NULL))

})


test_that("Run pairwise bootstrap comparisons", {

  data <- hydrafit::scof2012

  exp1_fits <- list()
  for(ii in unique(data$species)) {
    exp1_fits[[ii]] = fit_vuln_curve(kl ~ psi,
                                            data |> dplyr::filter(species == ii),
                                            model_type = "exp",
                                            plot=F)
  }

  attr(exp1_fits, "fit.list") <- TRUE

  bootstrap_list <- bootPX(exp1_fits, psi_max=0.1, seed= 202, pairwise = T)

  testthat::expect_equal(length(bootstrap_list), length(unique(data$species)))

  bootstrap_list2 <- bootPX(exp1_fits, psi_max=0.1, seed= 202, pairwise = T)

  # check that the bootstrap results are reproducible
  testthat::expect_identical(bootstrap_list, bootstrap_list2)

  # check the summary(has the correct number of values and outputs the PX value

  bootstrap_summary <- summary(bootstrap_list)
  testthat::expect_snapshot(bootstrap_summary)

})
