#' Define functions and parameter values
#'
#' @param A model parameter A
#' @param B model parameter B
#' @param Xo Xo parameter in the sigmoidal model
#' @param C C parameter in the Exponential 2 model
#' @param psi x value for the model
#'
#'
#' @return Returns models for estimating parameter values
#'
#' @details Used as templates for testing the fit of exponential,
#'    sigmoidal, logistic, exponential variant, and linear relationships.
#'
#' \bold{Model types:}
#'
#' \itemize{
#' \item Exponential
#' \item Exponential2
#' \item Logistic
#' \item Sigmoidal
#' \item Linear
#' }
#'
#' @rdname fitfunctions
#' @export

Exponential <- function (A, B, psi) {
  A * exp(-B * psi)
}

NULL
#' @rdname fitfunctions
#' @export
Logistic <- function (A, B, Xo, psi) {
  A / (1 + ((psi / Xo) ^ B))
}

NULL
#' @rdname fitfunctions
#' @export

Sigmoidal <- function (A, B, Xo, psi) {
  A / (1 + exp(-((psi - Xo) / B)))
}

NULL
#' @rdname fitfunctions
#' @export
Exponential2 <- function (A, B, C, psi) {
  C + A * exp(-B * psi)
}

NULL
#' @rdname fitfunctions
#' @export
Linear <- function (A, B, psi) {
  B * (psi) + A
}
