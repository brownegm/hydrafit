## Max C should equal the value from the fit when "max_cond_at"=0
 psi.px <- psiPx(fx_type = "Linear")
 A <- 2
 B <- 0.5

psi_at_p50_linear <- psi.px(A=A, B=B, px = 0.5, max_cond_at = 0)

psi_at_p50_linear
