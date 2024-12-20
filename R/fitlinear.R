#' fit_linear
#'
#' Fits linear parameters for species' data.
#'
#' @param input_df Input data frame that contains paired conductance (e.g., "kl") and leaf water potential observations
#' @param model_type Select appropriate model function. Here the default is linear
#' @param plot True or false for plotting model parameters
#' @param ... Additional plotting parameters if plot==T (e.g., xlab, ylab)
#'
#'
#' @return Returns best fitting model parameters for each species for nonlinear fits
#'
#' @importFrom stats dnorm lm
#' @importFrom graphics lines title
#' @export fit_linear
#'

fit_linear <- function(input_df,
                       model_type="Linear",
                       plot=F,
                       ...) {

  var <- list(psi="psi",
              x="kl",
              mean="predicted",
              log=TRUE)


par_estimates <- define_pars(input_df, model_type = model_type)

pars <- par_estimates[[1]]
par_lo <- par_estimates[[2]]
par_hi <- par_estimates[[3]]

model_function <- hydrafit::Linear

  res<-anneal(model = model_function, par = pars, source_data = input_df,
              var = var, par_lo = par_lo, par_hi = par_hi, dep_var = "kl",
              pdf = dnorm, max_iter=5000, show_display=F)


  #AIC formula: -2LL + 2*parameters (incl nuisance, i.e.,sd)

  AIC<- res$aic|>as.numeric()

  #AICcorr formula: -2LL + (2*n*parameters (incl nuisance, i.e.,sd)/(n-parameters-1))

  AICcorr <- res$aic_corr|>as.numeric()

  slope <-res$slope|>as.numeric()

  rsq <- res$R2|>as.numeric()

  sterror <- res$std_errs#the function anneal calculates the "standard errors" as
  #a the diagonal of the variance covariance matrix.Is this not then just the standard deviations
  #of the function's predicted parameters? See here:https://rdrr.io/cran/likelihood/src/R/anneal.R

  N <- length(res$source_data$kl)

  max_cond <- res$best_pars$A|>as.numeric()

  parvecLog<-structure(list(
               species = paste(input_df[1,1]),
               data.type = "Linear",
               A = res$best_pars[[1]]|>as.numeric(),
               B = res$best_pars[[2]]|>as.numeric(),
               C = res$best_pars[[3]]|>as.numeric(),
               D = NA,
               loglikeli = res$max_likeli|>as.numeric(),
               rsq = rsq,
               slope = slope,
               AIC = AIC,
               AICcorr = AICcorr,
               sterrorA = sterror[[1]]|>as.numeric(),
               sterrorB = sterror[[2]]|>as.numeric(),
               sterrorC = sterror[[3]]|>as.numeric(),
               sterrorD = NA,
               N = N,
               maxCond=max_cond|>as.numeric()),
               # attributes
               mod.type = "linear"
               )

  if(plot==T){

    plot(res$source_data$psi, res$source_data$kl)#, ...

    cbind(res$source_data$psi, res$source_data$predicted)-> for_plotting

    for_plotting[order(for_plotting[,1]),]-> for_plotting

    lines(for_plotting[,1], for_plotting[,2], col="blue")

    title(paste(input_df[1,1], input_df[1,2]))

  }

  return(parvecLog)

}



# lm(input_df$kl ~ input_df$psi)-> fita #a normal linear regression gave the best starting parameters
#
# pars = list(
#   A = summary(fita)$coeff[1, 1],
#   B = summary(fita)$coeff[2, 1],
#   sd = 1
# )
# par_lo1 = list(
#   A = summary(fita)$coeff[1, 1] * 0.1,
#   B = summary(fita)$coeff[2, 1] * 2,
#   sd = 0.005
# )
# par_hi1 = list(
#   A = summary(fita)$coeff[1, 1] * 2,
#   B = summary(fita)$coeff[2, 1] * 0.1,
#   sd = 20
#)
