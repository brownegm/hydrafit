#' Compute the best fit likelihood parameters for each species for each model type

#' @details See r/fitfunctions.R for functional types and see R/defineparams.R for parameter definitions.
#'
#' @param input_df input folder with kl and psi values
#' @param model_type select appropriate model type here i.e., "sig" for sigmoidal, "exp" and "exp2" for Exponentials and "log" for Logistic. See R/fitfunctions.R for functional types
#' @param plot True or false for plotting model parameters
#' @param ... Plotting parameters passed to \code{plot()} if plot=TRUE
#'
#'
#' @details This function utilizes the `anneal` function of the likelihood package \code{citation('likelihood')}. Within this function there are few assumptions about how we expect the annealing and fitting process is run:
#'
#' \itemize{
#' \item It looks weird that kl is the x variable here, but anneal calculates the slope and R2 of the fit using the predicted kl values as the y and the observed kl values as the x
#' \item The parameters are set to change slowly in the fitting procedure (the temp_red variable) helped a lot. You can watch the fitting proceed with show_display. \emph{This option needs to be change within the function itself. There is no parameter in the function at the moment to do this.}
#' \item AIC formula: -2LL + 2 x parameters (incl nuisance, i.e.,sd)
#' \item AICcorr formula: -2LL + (2n x parameters (incl nuisance, i.e.,sd)/(n-parameters-1))
#' }
#'
#' @return Returns best fitting model parameters for each species for nonlinear fits
#'
#' @importFrom likelihood anneal
#'
#' @export


fit_nonlinear <- function(input_df,
                          model_type,
                          plot = F, ...) {

  mod <- ifelse(model_type=="log", Logistic,
                        ifelse(model_type=="exp", Exponential,
                               ifelse(model_type=="exp2",Exponential2,
                                      ifelse(model_type=="sig", Sigmoidal))))

  par_estimates <- define_pars(input_df, model_type = model_type)

  pars = par_estimates[[1]]
  pars_low_i = par_estimates[[2]]
  pars_high_i = par_estimates[[3]]

    var <- list(
      psi = "psi",
      x = "kl",
      mean = "predicted",
      log = TRUE
    )

    res <-
      anneal(
        model = mod,
        par = pars,
        source_data = input_df,
        var = var,
        par_lo = pars_low_i,
        par_hi = pars_high_i,
        dep_var = "kl",
        pdf = dnorm,#pdf stands for probability density function
        max_iter = 5000,
        show_display = F,
        temp_red = 0.05
      )

    #Setting the parameters to change slowly in the fitting procedure (the temp_red variable)
    #helped a lot. You can watch the fitting proceed with show_display,
    #but I've never found it very informative

    #AIC formula: -2LL + 2*parameters (incl nuisance, i.e.,sd)

    AIC <- res$aic|>as.numeric()

    #AICcorr formula: -2LL + (2*n*parameters (incl nuisance, i.e.,sd)/(n-parameters-1))

    AICcorr <- res$aic_corr|>as.numeric()

    slope <- res$slope|>as.numeric()

    rsq <- res$R2|>as.numeric()

    sterror <- res$std_errs

    N <- length(res$source_data$kl)

    max_cond <- res$best_pars$A|>as.numeric()

    parvecLog <- structure(list(
      species = paste(input_df[1, 1]),
      data.type = model_type,
      A = res$best_pars[[1]]|>as.numeric(),
      B = res$best_pars[[2]]|>as.numeric(),
      C = res$best_pars[[3]]|>as.numeric(),
      D = res$best_pars[[4]]|>as.numeric(),
      loglikeli = res$max_likeli,
      rsq = rsq,
      slope = slope,
      AIC = AIC,
      AICcorr = AICcorr,
      sterrorA = sterror[[1]]|>as.numeric(),
      sterrorB = sterror[[2]]|>as.numeric(),
      sterrorC = sterror[[3]]|>as.numeric(),
      sterrorD = sterror[[4]]|>as.numeric(),
      N = N,
      maxCond=max_cond),
    # attributes
    mod.type = model_type
    )

    #plot the fit

    if (plot == T) {
      plot(res$source_data$psi,
           res$source_data$kl,
           ...)

      cbind(res$source_data$psi, res$source_data$predicted) -> for_plotting

      for_plotting[order(for_plotting[, 1]), ] -> for_plotting

      lines(for_plotting[, 1], for_plotting[, 2], col = "blue")

      title(paste(input_df[1, 1], input_df[1, 2], model_type))

    }
    return(parvecLog)

  }
