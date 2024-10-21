#' psiPX: Estimate plrc at a given maximum conductance
#'
#' @description
#' Creates a function that estimates the percent loss in conductance assuming maximum conductance at a chosen leaf water potential.
#' @details The functions within the functional are anonymous themselves, hence the "\(params)" structure.
#' @keywords internal
#' @param fx_type Function type. See ?Exponential for function options.
#'
#' @return Returns a function which takes model parameters,"px", and "max_cond_at" as parameters.
#'
#' @example
#' # create function for estimating pX with linear formula
#' psi.px <- psiPx(fx_type = "Linear")
#' A <- 2
#' B <- 0.5
#'
#' psi_at_p50_linear <- psi.px(A=A, B=B, px = 0.5, max_cond_at = 0)
#'
#' psi_at_p50_linear
#' @export

psiPx <- function(fx_type = character()) {

  if (fx_type == "Linear") {

    \(A, B, px = 0.5, max_cond_at = 0) {
      px_op <- 1-px

      max_c <- (B * max_cond_at) + A

      psi.px <- ((px_op * max_c) - A) / (B)

      return(list(psi.px, max_c))

    }

  } else if (fx_type == "exp") {
    \(A, B, px = 0.5, max_cond_at = 0) {

      px_op <- 1-px

      max_c <- A * exp(-B * max_cond_at)

      psi.px <- (log((px_op * max_c) / A)) / (-B)

      return(list(psi.px, max_c))
    }

  } else if (fx_type == "exp2") {
    \(A, B, C, px = 0.5, max_cond_at = 0) {

      px_op <- 1-px

      max_c <- C + (A * exp(-B * max_cond_at))

      psi_px <- -((log(((px_op * max_c) - C) / A)) / (B))

      return(list(psi_px, max_c))

    }

  } else if (fx_type == "log") {
    \(A, B, C, px = 0.5, max_cond_at = 0) {

      px_op <- 1-px

      max_c <- (A) / (1 + (max_cond_at / C) ^ B)

      psi_px <- (C) * (((A) / (px_op * max_c) - 1) ^ (1 / B))

      return(list(psi_px, max_c))
    }

  } else{
    #if sigmoidal

    \(A, B, C, px = 0.5, max_cond_at = 0) {

      px_op <- 1-px

      max_c <- (A) / (1 + exp(-((max_cond_at - C) / B)))

      psi_px <- -B * (A - (px_op * max_c) - log(px_op * max_c)) + C

      return(list(psi_px, max_c))

    }

  }

}
