
#' Compute the best fit likelihood parameters for each species for each model type
#' @details See r/fitfunctions.R for functional types and see R/defineparams.R for parameter definitions.
#' @param input_df input folder with kl and psi values
#' @param model_type select appropriate model type here i.e., Sigmoidal, Exponentials and Logistic. See R/fitfunctions.R for functional types
#' @param pars1 parameters set based on values in input df. See R/defineparams.R for parameter definitions.
#' @param par_lo1 parameter values set as lower bounds for estimates. See R/defineparams.R for parameter definitions.
#' @param par_hi1 parameter values set as upper bounds for estimates.
#' @param model_type_char select appropriate model type here
#' @param plot True or false for plotting model parameters
#'
#' @return Returns best fitting model parameters for each species for nonlinear fits
#' @export do_the_thing_nonlinear


do_the_thing_nonlinear <- function(input_df, model_type, pars1, par_lo1, par_hi1, model_type_char=character(), plot=F) {
  model_type = model_type
  var <- list(psi="psi",
              x="kl",
              mean="predicted",
              log=TRUE)
  #It looks weird that kl is the x variable here, but anneal calculates the slope and R2 of the fit using the predicted kl values as the y and the observed kl values as the x
  pars = pars1
  par_lo = par_lo1
  par_hi= par_hi1

  res<-anneal(model = model_type, par= pars1, source_data = input_df,
              var = var, par_lo=par_lo1, par_hi= par_hi1, dep_var = "kl",
              pdf = dnorm, max_iter=5000, show_display=F, temp_red=0.05)#pdf stands for probability density function

  #Setting the parameters to change slowly in the fitting procedure (the temp_red variable)
  #helped a lot. You can watch the fitting proceed with show_display,
  #but I've never found it very informative

  #AIC formula: -2LL + 2*parameters (incl nuisance, i.e.,sd)

  AIC<- res$aic

  #AICcorr formula: -2LL + (2*n*parameters (incl nuisance, i.e.,sd)/(n-parameters-1))

  AICcorr <- res$aic_corr

  slope <-res$slope

  rsq <- res$R2

  sterror<- res$std_errs

  N <- length(res$source_data$kl)

  gmax<-res$best_pars$A

  parvecLog<-c(Species= paste(input_df[1,1]),
               data.type=model_type_char,# data.type=paste(input_df[1,2]),
               A = res$best_pars[1], B = res$best_pars[2], C = res$best_pars[3], D = res$best_pars[4],
               loglikeli = res$max_likeli, rsq = rsq, slope = slope, AIC = AIC, AICcorr = AICcorr,
               sterror_1 = sterror[1], sterror = sterror[2], sterror = sterror[3], sterror = sterror[4],
               N =N,
               gmax=gmax)

  #plot the fit

  if(plot==T){

    plot(res$source_data$psi, res$source_data$kl, xlab = "Water Potential (-MPa)", ylab = "Stomatal Conductance (mmol m-2 s-1)")
    cbind(res$source_data$psi, res$source_data$predicted)-> for_plotting
    for_plotting[order(for_plotting[,1]),]-> for_plotting
    lines(for_plotting[,1], for_plotting[,2], col="blue")
    title(paste(input_df[1,1], input_df[1,2]))

  }
  return(parvecLog)

}
