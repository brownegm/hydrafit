test_that("bootstrap check ", {

data <- scof2012

# outputs for the fits
linear_fits <- list()
logistic_fits <- list()
sigmoidal_fits <- list()
exp1_fits <- list()
exp2_fits <- list()

# fit the models
suppressWarnings({
for (ii in seq_along(unique(data$species))){
  subset(data, data$species == unique(data$species)[ii], select = c(1:ncol(data))) -> data_by_sp

  linear_fits[[ii]] = hydrafit::fit_vuln_curve(input_df = data_by_sp, model_type = "Linear", max_cond_at = 0.1, plot = F)

  logistic_fits[[ii]] = fit_vuln_curve(data_by_sp, model_type = "log", plot=F)

  sigmoidal_fits[[ii]] = fit_vuln_curve(data_by_sp, model_type= "sig", plot=F)

  exp1_fits[[ii]] = fit_vuln_curve(data_by_sp, model_type = "exp", plot=F)

  exp2_fits[[ii]] = fit_vuln_curve(data_by_sp,model_type= "exp2", plot=F)
}
})
# run resample
best_model <- fx_select(linear_fits, logistic_fits, sigmoidal_fits, exp1_fits, exp2_fits)

# set parameters for the resamples
fit <- best_model[[1]]

psi_max = 0.1
px = 0.5
seed = 123
sims = 1000

fit_resample <- resamplePX(fit, px=px, seed=seed, sims=sims, psi_max = psi_max)






})
