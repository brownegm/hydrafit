#' Tool for estimating confidence intervals for percent loss in conductance.
#'
#' @details This function takes the best fit model chosen by `fx_select()` and estimates confidence intervals
#'  by re-sampling model parameters from a normal distribution based on mean and standard deviation. Note that
#'  the standard deviation used here is estimated as the square root of a hessian matrix created when the anneal
#'  functions tests for the best fit model.
#'
#'\itemize{
#'    \item Annealing function info: \url{https://tinyurl.com/annealingsim}
#'    \item Github page: \url{https://github.com/cran/likelihood/blob/master/R/anneal.R}. See lines 742-797 specifically within the code.
#' }

#' @param fit Fit object(s) from `fit_vuln_curve()`
#' @param px Percent loss in conductance. Default is 0.5.
#' @param psi_max Value of psi at which to estimate maximum conductance.
#' @param seed Value for reproducibility
#' @param sims Number of simulations to run. Default is 1000.
#' @param pairwise Perform pairwise test among species px values.
#'
#' @importFrom stats qt sd median
#' @export bootPX
#'
bootPX <- function(fit,
                   px = 0.5,
                   psi_max,
                   seed = 123,
                   sims = 1000,
                   pairwise = F,
                   margin = c("tdist", "quantile")) {

  # get margin method
  if (missing(margin)) {
    warning("No margin of error method provided. Defaulting to t-distribution reconstruction.")
    margin <- "tdist"
  } else{
    margin <- match.arg(margin)
  }
  # manage fit list vs single fit
  fit.list <- attr(fit, "fit.list")

  if (fit.list) {
    n_fit <- length(fit)
    model.test <- lapply(seq_len(n_fit), \(x) attr(fit[[x]], "mod.type")) %in% c("exp2", "log", "sig", "Linear", "exp")
  } else{
    n_fit <- 1
    model.test <- attr(fit, "mod.type") %in% c("exp2", "log", "sig", "Linear", "exp")
  }

  #check if the model type(s) is/are valid
  if (any(!model.test)) {
    print(model.test)
    stop("Bootstrap: Model type(s) must be one of the following: exp2, log, sig, Linear, exp")
  }

  #check if the percent loss in conductance is valid
  if (px > 1 | px < 0) {
    stop("Percent loss in conductance must be between 0 and 1")
  }

  #check if the psi_max value is valid
  if (psi_max < 0) {
    stop("Value for psi_max must be greater than 0")
  }

  px_char <- paste0(as.character(px), "@", as.character(psi_max))

  alpha = 0.05

  output <- vector(mode = "list", length = n_fit)

  for (i in seq_len(n_fit)) {
    if (fit.list) {
      fit_temp <- fit[[i]]
    } else{
      fit_temp <- fit
    }

    px_est <- switch(
      px_char,
      "0.5@" = fit_temp$psi_k50,
      "0.8@" = fit_temp$psi_k80,
      "0.5@0.1" = fit_temp$psi_k50_at0.1,
      "0.8@0.1" = fit_temp$psi_k80_at0.1
    )

    fit_resample <- resamplePX(
      fit = fit_temp,
      sims = sims,
      px = px,
      psi_max = psi_max,
      seed = seed
    )

    finite_values <- sapply(fit_resample$psi_px, function(x) is.finite(x[[1]]))

    boot_vals <- fit_resample$psi_px[finite_values] |> unlist()

    boot_mean <- mean(boot_vals, na.rm = T)

    boot_se <- se(boot_vals)

    boot_median <- median(boot_vals, na.rm = T)

    #determine confidence intervals
    if(margin == "quantile"){

      conf.int <- quantile(boot_vals,
                           probs = c(alpha / 2, 1 - (alpha / 2)),
                           na.rm = T)

      conf.low <- conf.int[1]
      conf.high <- conf.int[2]
      deg_of_freedom = (length(boot_vals) - 1)
      margin_error <- (conf.high - conf.low) / 2

    }else if (margin == "tdist"){
      # t-distribution method
    deg_of_freedom = length(boot_vals)
    t_score = qt(p = alpha / 2,
                 df = deg_of_freedom,
                 lower.tail = F)

    margin_error <- t_score * boot_se

    conf.low <- px_est - margin_error # using the predicted pX value to make the error make sense
    conf.high <- px_est + margin_error
    }

    # save out of the results
    output[[i]] <- structure(
      list(
        species = fit_temp$species,
        psi_PX = px_char,
        boot_mean = boot_mean,
        boot_median = boot_median,
        boot_se = boot_se,
        deg_of_freedom = deg_of_freedom,
        margin_error = margin_error,
        conf.low = conf.low,
        conf.high = conf.high,
        bootvals = boot_vals,
        model_params = fit_resample$model_params
      ))
  }

  if (pairwise == T) {
    # if pairwise comparisons are requested, then run them among the fits
    pw_out <- compare_sp_boot(output, conf_level = 0.95)
  }

  if (n_fit == 1) {
    return(structure(
      unlist(output, recursive = FALSE),
      pairwise_comp = if (pairwise == T)
        pw_out
      else
        NA,
      margin = margin,
      class = c("boot_list", "list")
    ))
  } else{
    return(structure(
      output,
      pairwise_comp = if (pairwise == T)
        pw_out
      else
        NA,
      margin = margin,
      class = c("boot_list", "list")
    ))
  }

}

#' Resample PX
#' @details Resampling is done by sampling from a normal distribution based on the mean and standard deviation of the parameter
#' @param fit Best fitting model containing best fit models, parameter estimates and their SDs
#' @param px Choose what percent loss in K/gs you are solving for PX (e.g., P50, P80 etc)
#' @param sims Number of simulated values to generate
#' @param psi_max estimate px based on what value of psi
#' @param seed Seed for reproducibility.
#'
#' @return Returns a list of simulations predicted PX values the length of the number `sims`.
#'
#' @importFrom stats rnorm
#' @importFrom withr with_seed


resamplePX <- function(fit,
                       #model_type = character(),
                       px = 0.5,
                       seed,
                       sims = 1000,
                       psi_max = numeric()) {
  if (length(psi_max) < 1) {
    stop("Value for psi_max must be provided.")
  }

  withr::local_seed(seed = seed)

  psi_px <- vector("list", length = sims) #initialize list to store results
  model_type <- fit$data.type

  #check conditions
  fx_with_param3 <- model_type %in% c("exp2", "log", "sig")

  #define model parameters
  A <- fit$A
  B <- fit$B

  #sd of parameter estimates
  A.sd <- fit$sterrorA
  B.sd <- fit$sterrorB

  if (fx_with_param3 == F) {
    # linear and exponential

    param_samples <- lapply(
      c(1:sims),
      #create X samples of paired values

      function(x) {
        lapply(1, function(y)
          c(
            sample(
              rnorm(sims, A, A.sd),
              size = 1,
              replace = T
            ),
            #sample for A
            sample(
              rnorm(sims, B, B.sd),
              size =
                1,
              replace = T
            ) #sample for B
          ))

      }
    )

  } else{
    #exponential2, logistic, and sigmoidal

    # define the third parameter for models with 3 parameters...C or Xo
    param_3 <- fit$C
    param_3.sd <- fit$sterrorC

    param_samples <- lapply(
      c(1:sims),
      #create X samples of paired values

      function(x) {
        lapply(1, function(y)
          c(
            sample(
              rnorm(sims, A, A.sd),
              size = 1,
              replace = T
            ),
            #sample for A
            sample(
              rnorm(sims, B, B.sd),
              size = 1,
              replace = T
            ),
            #sample for B
            sample(
              rnorm(sims, param_3, param_3.sd),
              size = 1,
              replace = T
            )
          ))#sample for Xo or C

      }
    )

  }

  psi_px_boot <- psiPx(model_type = model_type)

  if (fx_with_param3 == T) {
    for (i in 1:sims) {
      # this is a lot to look at!!!
      # Only way to index this list of lists since unlist makes this unusable
      # to be updated in future update

      psi_px[[i]] <- psi_px_boot(
        A = param_samples[[i]][[1]][1],
        B = param_samples[[i]][[1]][2],
        C = param_samples[[i]][[1]][3],
        px = px,
        max_cond_at = psi_max
      )$psi.px

    }
  } else{
    for (i in 1:sims) {
      psi_px[[i]] <- psi_px_boot(
        A = param_samples[[i]][[1]][1],
        B = param_samples[[i]][[1]][2],
        px = px,
        max_cond_at = psi_max
      )$psi.px

    }

  }#end for loop

  psi_px_out <- structure(list(psi_px = psi_px,
                               model_params = param_samples))

  return(psi_px_out)
}


#' Standard Error
#'
#' @param x Input value(s)
#' @param na.rm Remove NA values. Default is TRUE.
#' @returns The standard error of the input value(s).
#' @details na.rm=T by default because the predicted parameters estimate negative values and log(negative number)=NA
#'
se <- function(x, na.rm = T) {
  sd(x, na.rm = na.rm) / sqrt(length(x))
}


#' Print method for lists of bootstrap results
#'
#' @param x Object of class 'boot_list'
#' @param ... not used
#'
#' @returns Bootstrap results
#' @export

print.boot_list <- function(x, ...) {
  if (length(x) > 1) {
    for (i in 1:length(x)) {
      x_i <- x[[i]]
      cat("Bootstrap Results:\n")
      cat("----------------------------------------------------\n")
      cat("Species:", x_i$species, "\n")
      cat("PX:", x_i$psi_PX, "\n")
      cat("----------------------------------------------------\n")
      cat("Mean predicted PX:", x_i$boot_mean |> round(3), "\n")
      cat("Median predicted PX:", x_i$boot_median |> round(3), "\n")
      cat("Standard Error predicted PX:",
          x_i$boot_se |> round(3),
          "\n")
      cat("-----------------Confidence Interval----------------\n")
      cat("Low:", x_i$conf.low |> round(3), "\n")
      cat("High:", x_i$conf.high |> round(3), "\n")
      cat("\n\n")
    }
  } else {
    cat("Bootstrap Results:\n")
    cat("----------------------------------------------------\n")
    cat("Species:", x$species, "\n")
    cat("PX:", x$psi_PX, "\n")
    cat("----------------------------------------------------\n")
    cat("Mean predicted PX:", x$boot_mean |> round(3), "\n")
    cat("Median predicted PX:", x$boot_median |> round(3), "\n")
    cat("Standard Error predicted PX:", x$boot_se |> round(3), "\n")
    cat("-----------------Confidence Interval----------------\n")
    cat("Low:", x$conf.low |> round(3), "\n")
    cat("High:", x$conf.high |> round(3), "\n")
    cat("----------------------------------------------------\n")
  }
}


#' Summary Method for lists of bootstrap results
#'
#' @description Summarizes the bootstrap results for a list of bootstrapped models. This summary only applies to intergroup (e.g., among species) comparisons.
#' @param object Object of class 'boot_list'
#' @param ... not used
#'
#' @returns A summary of the bootstrap results, including species names, and pairwise comparisons.
#' @export

summary.boot_list <- function(object, ...) {
  if (!inherits(object, "boot_list")) {
    stop("Input must be of class 'boot_list'.")
  }

  # Get species names
  species_names <- sapply(object, \(boot) boot$species)
  species_names <- paste(species_names, collapse = ", ")

  # What PX Value was bootstrapped?
  px <- unique(sapply(object, \(boot) boot$psi_PX))

  # Print summary of bootstrap results
  cat("Bootstrap Pairwise Summary:\n")
  cat("----------------------------------------------------\n")
  cat("Species:", species_names, "\n")
  cat("PX:", px, "\n")
  cat("----------------------------------------------------\n")
  print(attr(object, "pairwise_comp"))

}
