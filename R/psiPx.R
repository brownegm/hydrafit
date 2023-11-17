#' psiPX: Estimate plrc at a given maximum conductance
#'
#' @description
#' Creates a function that estimates the percent loss in conductance assuming maximum conductance at a chosen leaf water potential.
#' @details The functions within the functional are anonymous themselves, hence the "\(params)" structure.
#'
#' @param fx_type Function type. See ?Exponential for function options.
#'
#' @return Returns a function which takes model parameters,"px", and "max_cond_at" as parameters.
#'

psiPx <- function(fx_type = character()) {

  if (fx_type == "Linear") {
    \(A, B, px = 0.5, max_cond_at = 0) {
      max_c <- (B * max_cond_at) + A

      psi.px <- ((px * max_c) - A) / (B)

      return(psi.px)

    }

  } else if (fx_type == "exp") {
    \(A, B, px = 0.5, max_cond_at = 0) {

      max_c <- A * exp(-B * max_cond_at)

      psi.px <- (log((px * max_c) / A)) / (-B)

      return(psi.px)
    }

  } else if (fx_type == "exp2") {
    \(A, B, C, px = 0.5, max_cond_at = 0) {

      max_c <- C + (A * exp(-B * max_cond_at))

      psi_px <- -((log((( px * max_c) - C) / A)) / (B))

      return(psi_px)

    }

  } else if (fx_type == "log") {
    \(A, B, C, px = 0.5, max_cond_at = 0) {

      max_c <- (A) / (1 + (max_cond_at / C) ^ B)

      psi_px <- (C) * (((A) / (px * max_c) - 1) ^ (1 / B))

      return(psi_px)
    }

  } else{
    #if sigmoidal

    \(A, B, C, px = 0.5, max_cond_at = 0) {

      max_c <- (A) / (1 + exp(-((max_cond_at - C) / B)))

      psi_px <- -B * (A - (px * max_c) - log(px * max_c)) + C

      return(psi_px)

    }

  }

}
