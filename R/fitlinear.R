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

fit_linear <- function(input_df, model_type=hydrafit::Linear(), plot=F, ...) {

  model_type = model_type

  var <- list(psi="psi",
              x="kl",
              mean="predicted",
              log=TRUE)

  lm(input_df$kl ~ input_df$psi)-> fita #a normal linear regression gave the best starting parameters

  pars = list(A=summary(fita)$coeff[1,1],
              B =summary(fita)$coeff[2,1],
              sd = 1)
  par_lo1= list(A=summary(fita)$coeff[1,1]*0.1,
                B =summary(fita)$coeff[2,1]*2,
                sd =0.005)
  par_hi1= list(A=summary(fita)$coeff[1,1]*2,
                B =summary(fita)$coeff[2,1]*0.1,
                sd =20)
  #########Update from Megan: I decreased the sd of the par_lo1, linear fits now match the lm results

  res<-anneal(model = model_type, par= pars, source_data = input_df,
              var = var, par_lo=par_lo1, par_hi= par_hi1, dep_var = "kl",
              pdf = dnorm, max_iter=5000, show_display=F)


  #AIC formula: -2LL + 2*parameters (incl nuisance, i.e.,sd)

  AIC<- res$aic

  #AICcorr formula: -2LL + (2*n*parameters (incl nuisance, i.e.,sd)/(n-parameters-1))

  AICcorr <- res$aic_corr

  slope <-res$slope

  rsq <- res$R2

  sterror <- res$std_errs#the function anneal calculates the "standard errors" as
  #a the diagonal of the variance covariance matrix.Is this not then just the standard deviations
  #of the function's predicted parameters? See here:https://rdrr.io/cran/likelihood/src/R/anneal.R

  N <- length(res$source_data$kl)

  max_cond <- res$best_pars$A

  parvecLog<-structure(c(species = paste(input_df[1,1]),
               data.type="Linear",
               A = res$best_pars[1],
               B = res$best_pars[2],
               C = res$best_pars[3],
               D = res$best_pars[4],
               loglikeli = res$max_likeli,
               rsq = rsq,
               slope = slope,
               AIC = AIC,
               AICcorr = AICcorr,
               sterrorA = sterror[1],
               sterrorB = sterror[2],
               sterrorC = sterror[3],
               sterrorD = sterror[4],
               N = N,
               gmax=gmax),
               # attributes
               mod.type = "linear",
               class = "modfit"
               )

  if(plot==T){

    plot(res$source_data$psi, res$source_data$kl, ...)

    cbind(res$source_data$psi, res$source_data$predicted)-> for_plotting

    for_plotting[order(for_plotting[,1]),]-> for_plotting

    lines(for_plotting[,1], for_plotting[,2], col="blue")

    title(paste(input_df[1,1], input_df[1,2]))

  }

  return(parvecLog)

}
