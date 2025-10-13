

#' Get var-cov element for annealling
#'
#' @description Helper to get variance covariance matrix from anneal
#'
#' @details
#' pulls vcov , aligns it to mu_names (A,B[,C (or Xo)]),
#' repairs to PD, or falls back to diagonal from standard error estimtes
#'
#' @param fit
#' @param mu_names
#'
#' @returns Variance-covariance matrix

get_vcov_from_anneal <- function(fit, mu_names) {

  # ---- helpers ----
  # align the vcov matrix with the number of parameters per model and use their
  # names to define the matrix.
  align_to <- function(Sigma, mu_names) {
    p <- length(mu_names)
    Sigma <- as.matrix(Sigma)
    # try name-based selection first (case-insensitive, C/X0 synonyms)
    cn <- colnames(Sigma)
    rn <- rownames(Sigma)

    if (ncol(Sigma) >= p) {
      # Trim the variance covariance matrix to the number of parameters in the model,
      Sigma <- Sigma[seq_len(p), seq_len(p), drop = FALSE]

      dimnames(Sigma) <- list(mu_names, mu_names)
    } else {

      stop("vcov has fewer columns than parameters; cannot align.")
    }
    return(Sigma)
  }

  check_pd <- function(Sigma) {
    Sigma <- (Sigma + t(Sigma)) / 2
    ok <- tryCatch({
      # compute eigen vector from Sigma
      ev <- eigen(Sigma, symmetric = TRUE, only.values = TRUE)$values
      all(is.finite(ev)) && min(ev) > 0
    }, error = function(e)
      FALSE
    )
    if (!ok) {
      #estimate the nearest possible definite matrix from vcov if nonfinite
      Sigma <- as.matrix(Matrix::nearPD(Sigma)$mat) |>
        align_to(Sigma=_, mu_names = mu_names)

    }
    return(Sigma)
  }

  # get vcov, check names and
  Sigma <- NULL
  vcov_candidate <- fit$vcov
  if (!is.null(vcov_candidate)) {
      Sigma <- tryCatch(
        align_to(vcov_candidate, mu_names),
        error = function(e)
          NULL
      )
  }
  check_pd(Sigma)
}

# resample --------------------------------------------------------------------

resamplePX_new <- function(fit,
                           px = 0.5,
                           seed,
                           sims = 1000,
                           psi_max = numeric()) {

  if (length(psi_max) < 1) {
    stop("Value for psi_max must be provided.")
  }

  withr::local_seed(seed = seed)

  model_type <- fit$data.type
  fx_with_param3 <- model_type %in% c("exp2", "log", "sig")

  # means (with names)
  mu <- if (fx_with_param3) {
    c(A = fit$A, B = fit$B, C = fit$C)
  } else {
    c(A = fit$A, B = fit$B)
  }

  # attempt to get vcov matrix
  Sigma <- get_vcov_from_anneal(fit, mu_names = names(mu))

  #sample from multivariate normal distribution
  param_samples <- MASS::mvrnorm(n = sims, mu = mu, Sigma = Sigma)

  # initialize predictions
  predictions_px <- vector("list", length = sims)
  psi_px_boot <- psiPx(model_type = model_type)

  if (fx_with_param3) {
    for (i in seq_len(sims)) {
      val <- psi_px_boot(
        A = param_samples[i, 1],
        B = param_samples[i, 2],
        C = param_samples[i, 3],
        px = px,
        max_cond_at = psi_max
      )$psi.px

      predictions_px[[i]] <- val
    }

  } else {
    for (i in seq_len(sims)) {
      val <- psi_px_boot(
        A = param_samples[i, 1],
        B = param_samples[i, 2],
        px = px,
        max_cond_at = psi_max
      )$psi.px

      predictions_px[[i]] <- val
    }
  }

  out_resample <- structure(list(
    psi_px = unlist(predictions_px, use.names = FALSE),
    model_params = param_samples,
    seed = seed
  ))

  return(out_resample)

}

test_new <- resamplePX(fit = fit, px = 0.5, seed = 123, psi_max = 0.1)
