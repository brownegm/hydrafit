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
#' \item The parameters are set to change slowly in the fitting procedure (the temp_red variable) helped a lot. You can watch the fitting proceed with show_display. \emph{This option needs to be changed within the function itself. There is no parameter in the function at the moment to do this.}
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

  mod <- ifelse(model_type=="log", hydrafit::Logistic,
                        ifelse(model_type=="exp", hydrafit::Exponential,
                               ifelse(model_type=="exp2",hydrafit::Exponential2,
                                      ifelse(model_type=="sig", hydrafit::Sigmoidal, hydrafit::Linear))))

  par_estimates <- define_pars(input_df, model_type = model_type)

  pars = par_estimates[[1]]
  pars_low = par_estimates[[2]]
  pars_high = par_estimates[[3]]

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
        par_lo = pars_low,
        par_hi = pars_high,
        dep_var = "kl",
        pdf = dnorm,#pdf stands for probability density function
        max_iter = 8000,
        show_display = F,
        temp_red = 0.001, initial_temp = 100
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

    D <- ifelse(model_type=="exp", NA, res$best_pars$D|>as.numeric())
    D.se <- ifelse(model_type=="exp", NA, sterror[[4]]|>as.numeric())

    # create function to calculate water potential at X% max conductance
    px_fx <- hydrafit:::psiPx(fx_type = model_type)

    if(model_type =="Linear"){
    est_params <- list(A=A,
                       B=B)
    }else{
      est_params <- list(A=A,
                         B=B,
                         C=C)
    }

    parvecLog <- structure(list(
      species = paste(input_df[1, 1]),
      data.type = model_type,
      A = res$best_pars[[1]]|>as.numeric(),
      B = res$best_pars[[2]]|>as.numeric(),
      C = res$best_pars[[3]]|>as.numeric(),
      D = D,
      loglikeli = res$max_likeli,
      rsq = rsq,
      slope = slope,
      AIC = AIC,
      AICcorr = AICcorr,
      sterrorA = sterror[[1]]|>as.numeric(),
      sterrorB = sterror[[2]]|>as.numeric(),
      sterrorC = sterror[[3]]|>as.numeric(),
      sterrorD = D.se,
      N = N,
      maxCond = max_cond,
      psi_k20 = px_fx(A, B, C, px = 0.20)[[1]],
      psi_k50 = px_fx(A, B, C, px = 0.50)[[1]],
      psi_k80 = px_fx(A, B, C, px = 0.80)[[1]],
      psi_k95 = px_fx(A, B, C, px = 0.95)[[1]],
      max_cond_at0.1 = px_fx(A, B, C, max_cond=0.1)[[2]],
      psi_k20_at0.1 = px_fx(A, B, C, px = 0.20, max_cond=0.1)[[1]],
      psi_k50_at0.1 = px_fx(A, B, C, px = 0.50, max_cond=0.1)[[1]],
      psi_k80_at0.1 = px_fx(A, B, C, px = 0.80, max_cond=0.1)[[1]],
      psi_k95_at0.1 = px_fx(A, B, C, px = 0.95, max_cond=0.1)[[1]]
      ),
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

#' @rdname fit_cond_model

px_estimates <- function(params, px_fx,
                         px = list(0.20,0.50,0.80,0.95),
                         max_cond_at = 0.1){

  pl <- list()
  pl_atmaxcond <- list()

  for (kx in seq_along(px)){

  params["px"] <- px[[kx]]

  pl[kx] <- do.call(px_fx, params)[["psi.px"]]

  params["max_cond_at"] <- max_cond_at

  pl_atmaxcond[kx] <- do.call(px_fx, params)[["psi.px"]]

  params$max_cond_at <- NULL

  }

  pl_output <- structure(list(
  p20 = pl[[1]],
  p50 = pl[[2]],
  p80 = pl[[3]],
  p95 = pl[[4]],
  p20_atmaxcond = pl_atmaxcond[[1]],
  p50_atmaxcond = pl_atmaxcond[[2]],
  p80_atmaxcond = pl_atmaxcond[[3]],
  p95_atmaxcond = pl_atmaxcond[[4]]
  ))

  return(pl_output)
}
