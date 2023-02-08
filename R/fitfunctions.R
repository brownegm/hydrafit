#' Define functions and parameter values
#'
#' @param A model parameter A
#' @param B model parameter B
#' @param Xo Xo parameter in the model
#' @param psi x value for the model
#'
#'
#' @return Returns models for estimating parameter values
#'
#'
Exponential <- function (A, B, psi) {
  A * exp(-B * psi)
}

Logistic <- function (A, B, Xo, psi) {
  A / (1 + ((psi / Xo) ^ B))
}

Sigmoidal <- function (A, B, Xo, psi) {
  A / (1 + exp(-((psi - Xo) / B)))
}

Exponential2 <- function (A, B, C, psi) {
  C + A * exp(-B * psi)
}

Linear <- function (A, B, psi) {
  B * (psi) + A
}
