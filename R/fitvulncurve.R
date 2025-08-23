#' Compute the best fit likelihood parameters for each species for each model type

#' @details See r/fitfunctions.R for functional types and see R/defineparams.R for parameter definitions.
#' @param formula Formula indicating the
#' @param input_df input folder with kl and psi values
#' @param model_type select appropriate model type here i.e., "Linear" for linear, "sig" for sigmoidal, "exp" and "exp2" for Exponentials and "log" for Logistic. See R/fitfunctions.R for functional types
#' @param max_cond_at Water potential which pX should be based upon.
#' @param bootstrap True or false for bootstrapping of P50 at your chosen max_cond_at
#' @param plot True or false for plotting model parameters
#' @param pdf probability density function. Default is dnorm.
#'
#' @details This function utilizes the `anneal` function of the likelihood package \code{citation('likelihood')}. This function will always return the hydraulic parameters assuming that maximum conductance is at 0 MPa. However, it may be more physiologically relevant for parameters to be estimated at 0.1MPa due to the presence of a driving force for hydraulic conductance. Within this function there are few assumptions about how we expect the annealing and fitting process is run:
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
#' @export
#' @importFrom stats dnorm
#' @importFrom graphics lines title
#' @importFrom ggplot2 ggplot geom_point geom_line labs
#' @importFrom rlang .data


fit_vuln_curve <- function(formula,
                           input_df,
                           model_type,
                           max_cond_at = 0.1,
                           pdf = stats::dnorm,
                           bootstrap = F,
                           plot = F) {
  if (!inherits(formula, "formula") | is.null(formula)) {
    stop("Formula is not of class 'formula' or is not provided")
  }

  if (!model_type %in% c("log", "exp", "exp2", "sig", "Linear")) {
    stop("Model type not in expected options.")
  }

  if (max_cond_at %in% c(NULL, 0)) {
    stop(
      "max_cond_at parameter is either not provided or equals zero.\n Models default to 0, so max_cond_at must be > 0."
    )
  }

  mod <- switch(
    model_type,
    "log" = hydrafit::Logistic,
    "exp" = hydrafit::Exponential,
    "exp2" = hydrafit::Exponential2,
    "sig" = hydrafit::Sigmoidal,
    hydrafit::Linear
  )

  if (!is.function(mod)) {
    stop("model is not a function.\n")
  }

  input_variables <- all.vars(formula)

  var_check <- all(input_variables %in% names(input_df))

  if (!var_check) {
    v <- which(!input_variables %in% names(input_df))
    if (length(v) > 1) {
      stop(
        paste(
          "'",
          input_variables[v[1]],
          "'",
          "and",
          "'",
          input_variables[v[2]],
          "'",
          "are not found in input dataframe."
        )
      )
    }
    stop(paste("'", input_variables[v], "'", "is not found in input dataframe."))
  }

  par_estimates <- define_pars(input_df, model_type = model_type)

  pars = par_estimates[[1]]
  pars_low = par_estimates[[2]]
  pars_high = par_estimates[[3]]

  var <- list(
    psi = input_variables[2],
    # independent variable
    x = input_variables[1],
    # dependent variable
    mean = "predicted",
    log = TRUE
  )

  res <- tryCatch({
    result <- suppressWarnings({
      # suppress and control NaN warnings
      anneal(
        model = mod,
        par = pars,
        source_data = input_df,
        var = var,
        par_lo = pars_low,
        par_hi = pars_high,
        dep_var = input_variables[1],
        pdf = dnorm,
        max_iter = 5000,
        temp_red = 0.8
      )
    })
    result
  }, warning = function(w) {
    message("Warning caught during annealing: ", conditionMessage(w))
    return(result)  # or NULL or a custom result object
  }, error = function(e) {
    message("Error during annealing: ", conditionMessage(e))
    return(NA)  # or NULL or a custom result object
  })

  #Setting the parameters to change slowly in the fitting procedure (the temp_red variable)
  #helped a lot. You can watch the fitting proceed with show_display,
  #but I've never found it very informative

  # organize output
  A = res$best_pars[[1]] |> as.numeric()
  B = res$best_pars[[2]] |> as.numeric()
  C = res$best_pars[[3]] |> as.numeric()
  #AIC formula: -2LL + 2*parameters (incl nuisance, i.e.,sd)

  AIC <- res$aic |> as.numeric()

  #AICcorr formula: -2LL + (2*n*parameters (incl nuisance, i.e.,sd)/(n-parameters-1))

  AICcorr <- res$aic_corr |> as.numeric()

  slope <- res$slope |> as.numeric()

  rsq <- res$R2 |> as.numeric()

  sterror <- res$std_errs

  N <- length(res$source_data$kl)

  max_cond <- res$best_pars$A |> as.numeric()

  D <- ifelse(model_type %in% c("exp", "Linear"),
              NA,
              res$best_pars$D |> as.numeric())
  D.se <- ifelse(model_type %in% c("exp", "Linear"),
                 NA,
                 sterror[[4]] |> as.numeric())

  # create function to calculate water potential at X% max conductance
  px_fx <- hydrafit::psiPx(model_type = model_type)

  if (model_type %in% c("exp", "Linear")) {
    est_params <- list(A = A, B = B)
  } else{
    est_params <- list(A = A, B = B, C = C)
  }

  px_estimates <- estimate_pxs(params = est_params,
                               px_fx = px_fx,
                               max_cond_at = max_cond_at)

  parlist_out <- structure(
    list(
      species = paste(input_df[1, 1]),
      data.type = model_type,
      A = A,
      B = B,
      C = C,
      D = D,
      loglikeli = res$max_likeli,
      rsq = rsq,
      slope = slope,
      AIC = AIC,
      AICcorr = AICcorr,
      sterrorA = sterror[[1]] |> as.numeric(),
      sterrorB = sterror[[2]] |> as.numeric(),
      sterrorC = sterror[[3]] |> as.numeric(),
      sterrorD = D.se,
      N = N,
      maxCond = max_cond,
      psi_k20 = px_estimates$p20,
      psi_k50 = px_estimates$p50,
      psi_k80 = px_estimates$p80,
      psi_k95 = px_estimates$p95,
      max_cond_at0.1 = px_estimates$max_c_atmaxpsi,
      psi_k20_at0.1 = px_estimates$p20_atmaxcond,
      psi_k50_at0.1 = px_estimates$p50_atmaxcond,
      psi_k80_at0.1 = px_estimates$p80_atmaxcond,
      psi_k95_at0.1 = px_estimates$p95_atmaxcond
    ),
    # attributes
    mod.type = model_type,
    fit.list = FALSE
  )

  if (bootstrap == T) {
    par_bootstrap <- bootPX(parlist_out, px = 0.5, psi_max = 0.1)
    parlist_out$conf.low <- par_bootstrap$conf.low
    parlist_out$conf.high <- par_bootstrap$conf.high
  }
  #plot the fit

  if (plot == T) {
    plotdata <- res$source_data

    fitplot <- ggplot2::ggplot(ggplot2::aes(x = .data$psi, y = .data$kl), data =
                                 plotdata) +
      ggplot2::geom_point(size = 2) +
      ggplot2::geom_line(ggplot2::aes(x = .data$psi, y = .data$predicted), color =
                           "red") +
      ggplot2::labs(title = parlist_out$species,
                    subtitle = paste("Model:", model_type)) +
      ggplot2::theme_classic(base_size = 14)

    parlist_out$plot <- fitplot
  }
  return(parlist_out)

}

#' Estimate water potential at given percent loss of conductivity
#'
#' @param params parameters list
#' @param px_fx function to calculate water potential at given percent loss of conductivity
#' @param px percent loss of conductivity values to estimate (default is 20, 50, 80, 95%)
#' @param max_cond_at water potential at which maximum conductance is assumed (default is 0.1 MPa)
estimate_pxs <- function(params,
                         px_fx,
                         px = list(0.20, 0.50, 0.80, 0.95),
                         max_cond_at = 0.1) {
  pl <- list()
  pl_atmaxcond <- list()

  for (kx in seq_along(px)) {
    params["px"] <- px[[kx]]

    pl[kx] <- do.call(px_fx, params)[["psi.px"]]

    params["max_cond_at"] <- max_cond_at

    pl_atmaxcond[kx] <- do.call(px_fx, params)[["psi.px"]]
    maxc_atmaxpsi <- do.call(px_fx, params)[["max_c"]]

    params$max_cond_at <- NULL

  }

  pl_output <- structure(
    list(
      p20 = pl[[1]],
      p50 = pl[[2]],
      p80 = pl[[3]],
      p95 = pl[[4]],
      p20_atmaxcond = pl_atmaxcond[[1]],
      p50_atmaxcond = pl_atmaxcond[[2]],
      p80_atmaxcond = pl_atmaxcond[[3]],
      p95_atmaxcond = pl_atmaxcond[[4]],
      max_c_atmaxpsi = maxc_atmaxpsi
    )
  )

  return(pl_output)
}
