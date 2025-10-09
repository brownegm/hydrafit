# # pulls vcov (or inverts Hessian), aligns it to mu_names (A,B[,C]),
# # repairs to PD, or falls back to diagonal from standard error estimtes
# get_vcov_from_anneal <- function(fit, mu_names, ses = NULL) {
#   # ---- helpers (scoped) ----
#   norm_par <- function(x) {
#     x <- tolower(trimws(x))
#     x <- sub("^x0$", "c", x)
#     x <- sub("^xo$", "c", x)
#     x
#   }
#   align_to <- function(Sigma, mu_names) {
#     p <- length(mu_names)
#     Sigma <- as.matrix(Sigma)
#     # try name-based selection first (case-insensitive, C/X0/XO synonyms)
#     cn <- colnames(Sigma)
#     rn <- rownames(Sigma)
#     if (!is.null(cn) && !is.null(rn)) {
#       want <- norm_par(mu_names)
#       have_c <- norm_par(cn)
#       have_r <- norm_par(rn)
#       if (all(want %in% have_c) && all(want %in% have_r)) {
#         idx <- match(want, have_c)
#         Sigma <- Sigma[idx, idx, drop = FALSE]
#       } else if (ncol(Sigma) >= p) {
#         # unnamed or partial naming: assume nuisance (e.g., sigma) at end → take leading p×p
#         Sigma <- Sigma[seq_len(p), seq_len(p), drop = FALSE]
#       } else {
#         stop("vcov has fewer columns than parameters; cannot align.")
#       }
#     } else {
#       if (ncol(Sigma) >= p) {
#         Sigma <- Sigma[seq_len(p), seq_len(p), drop = FALSE]
#       } else {
#         stop("Unnamed vcov has fewer columns than parameters; cannot align.")
#       }
#     }
#     dimnames(Sigma) <- list(mu_names, mu_names)
#     Sigma
#   }
#   repair_pd <- function(Sigma) {
#     Sigma <- (Sigma + t(Sigma)) / 2
#     ok <- tryCatch(
#       {
#         ev <- eigen(Sigma, symmetric = TRUE, only.values = TRUE)$values
#         all(is.finite(ev)) && min(ev) > 0
#       },
#       error = function(e) FALSE
#     )
#     if (!ok) {
#       Sigma <- as.matrix(Matrix::nearPD(Sigma)$mat)
#     }
#     Sigma
#   }
#   diag_from_ses <- function(mu_names, ses) {
#     if (is.null(ses) || length(ses) != length(mu_names)) {
#       stop("No usable vcov/Hessian and no matching ses fallback.")
#     }
#     D <- diag(ses^2)
#     dimnames(D) <- list(mu_names, mu_names)
#     D
#   }
#
#   # ---- try a direct vcov slot ----
#   Sigma <- NULL
#   vcov_candidates <- list(fit$vcov, attr(fit, "vcov", exact = TRUE))
#   for (v in vcov_candidates) {
#     if (!is.null(v)) {
#       Sigma <- tryCatch(align_to(v, mu_names), error = function(e) NULL)
#       if (!is.null(Sigma)) break
#     }
#   }
#
#   # ---- otherwise, try Hessian inverse ----
#   if (is.null(Sigma)) {
#     H <- NULL
#     H_candidates <- list(fit$hessian, attr(fit, "hessian", exact = TRUE))
#     for (h in H_candidates) {
#       if (!is.null(h)) {
#         H <- as.matrix(h)
#         break
#       }
#     }
#     if (!is.null(H)) {
#       H <- (H + t(H)) / 2
#       # try solve(H), then solve(-H) if needed (sign conventions vary)
#       vc_try <- tryCatch(solve(H), error = function(e) NULL)
#       if (is.null(vc_try)) {
#         vc_try <- tryCatch(solve(-H), error = function(e) NULL)
#       }
#       if (!is.null(vc_try)) {
#         Sigma <- tryCatch(align_to(vc_try, mu_names), error = function(e) NULL)
#       }
#     }
#   }
#
#   # ---- fall back to diagonal from ses if needed ----
#   if (is.null(Sigma)) {
#     Sigma <- diag_from_ses(mu_names, ses)
#     warning("Using diagonal covariance (no usable vcov/Hessian).")
#   }
#
#   repair_pd(Sigma)
# }
#
# # robust SE (uses non-NA length)
# se <- function(x) {
#   x2 <- x[is.finite(x)]
#   stats::sd(x2) / sqrt(length(x2))
# }
#
# # resample --------------------------------------------------------------------
#
# resamplePX <- function(fit, px = 0.5, seed, sims = 1000, psi_max = numeric()) {
#   if (length(psi_max) < 1) {
#     stop("Value for psi_max must be provided.")
#   }
#   withr::local_seed(seed = seed)
#
#   model_type <- fit$data.type
#   fx_with_param3 <- model_type %in% c("exp2", "log", "sig")
#
#   # means (with names)
#   mu <- if (fx_with_param3) {
#     c(A = fit$A, B = fit$B, C = fit$C)
#   } else {
#     c(A = fit$A, B = fit$B)
#   }
#
#   #standard error estimates (ses) for fallback
#   ses <- if (fx_with_param3) {
#     c(fit$sterrorA, fit$sterrorB, fit$sterrorC)
#   } else {
#     c(fit$sterrorA, fit$sterrorB)
#   }
#   # # attempt to get vcov (from vcov or inverse Hessian)
#   # Sigma <- get_vcov_from_anneal(fit)
#   # <<< single call that fetches + aligns + repairs >>>
#   Sigma <- get_vcov_from_anneal(fit, mu_names = names(mu), ses = ses)
#
#   draws <- MASS::mvrnorm(n = sims, mu = mu, Sigma = Sigma)
#
#   Here
#
#   psi_px <- vector("list", length = sims)
#   psi_px_boot <- psiPx(model_type = model_type)
#
#   if (fx_with_param3) {
#     for (i in seq_len(sims)) {
#       val <- psi_px_boot(
#         A = draws[i, 1],
#         B = draws[i, 2],
#         C = draws[i, 3],
#         px = px,
#         max_cond_at = psi_max
#       )$psi.px
#
#       if (!is.finite(val)) {
#         invalid <- invalid + 1L
#       }
#
#       psi_px[[i]] <- val
#     }
#   } else {
#     for (i in seq_len(sims)) {
#       val <- psi_px_boot(
#         A = draws[i, 1],
#         B = draws[i, 2],
#         px = px,
#         max_cond_at = psi_max
#       )$psi.px
#
#       if (!is.finite(val)) {
#         invalid <- invalid + 1L
#       }
#       psi_px[[i]] <- val
#     }
#   }
#
#   sims <- draws
#   sims$psi_px <- psi_px
#
#   structure(list(
#     psi_px = psi_px,
#     model_params = split.data.frame(draws, seq_len(nrow(draws)))
#   ))
# }
#
#
# invalid <- 0L
#
# if (fx_with_param3) {
#   for (i in seq_len(sims)) {
#     val <- psi_px_fun(
#       A = draws[i, "A"],
#       B = draws[i, "B"],
#       C = draws[i, "C"],
#       px = px,
#       max_cond_at = psi_max
#     )$psi.px
#     if (!is.finite(val)) {
#       invalid <- invalid + 1L
#     }
#     psi_px[[i]] <- val
#   }
# } else {
#   for (i in seq_len(sims)) {
#     val <- psi_px_fun(
#       A = draws[i, "A"],
#       B = draws[i, "B"],
#       px = px,
#       max_cond_at = psi_max
#     )$psi.px
#     if (!is.finite(val)) {
#       invalid <- invalid + 1L
#     }
#     psi_px[[i]] <- val
#   }
# }
