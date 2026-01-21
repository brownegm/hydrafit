# tests/testthat/test-estimate_pxs.R

test_that("estimate_pxs returns expected structure and values", {
  # Behaviors:
  # - psi.px increases with px (because px_op = 1 - px, so smaller px_op -> larger psi)
  # - max_cond_at shifts psi.px upward
  px_fx_stub <- function(A, B, C, px = 0.5, max_cond_at = 0) {
    px_op <- 1 - px
    max_c <- A / (1 + (max_cond_at / C)^B)
    psi.px <- C * ((A / (px_op * max_c) - 1)^(1 / B))

    list(psi.px = psi.px, max_c = max_c)
  }

  params <- list(A = 1, B = 2, C = 3)

  out <- estimate_pxs(
    params = params,
    px_fx = px_fx_stub,
    px = c(0.20, 0.50, 0.80, 0.95),
    max_cond_at = 0.1
  )

  testthat::expect_type(out, "list")

  expected_names <- c(
    "p20", "p50", "p80", "p95",
    "p20_atmaxcond", "p50_atmaxcond", "p80_atmaxcond", "p95_atmaxcond",
    "max_c_atmaxpsi"
  )
  expect_named(out, expected_names)

  # all numeric scalars
  testthat::expect_true(all(vapply(out, function(x) is.numeric(x) && length(x) == 1, logical(1))))

  # monotonicity across px (given this stub)
  testthat::expect_lt(out$p20, out$p50)
  testthat::expect_lt(out$p50, out$p80)
  testthat::expect_lt(out$p80, out$p95)

  # max_cond_at should change the psi values relative to baseline for each px
  testthat::expect_true(out$p20_atmaxcond != out$p20)
  testthat::expect_true(out$p50_atmaxcond != out$p50)
  testthat::expect_true(out$p80_atmaxcond != out$p80)
  testthat::expect_true(out$p95_atmaxcond != out$p95)

  # "max_c_atmaxpsi" should equal max_c computed with px = last(px) and max_cond_at
  last_px <- 0.95
  expected_maxc <- px_fx_stub(A = 1, B = 2, C = 3, px = last_px, max_cond_at = 0.1)$max_c
  expect_equal(out$max_c_atmaxpsi, expected_maxc)
})

test_that("px_fx actually responds to max_cond_at at px=0.5 (should change psi or max_c)", {
  params <- list(A = 1, B = 2, C = 3)

  px_fx <- psiPx(model_type = "sig")
  out0  <- do.call(px_fx, c(params, list(px = 0.5, max_cond_at = 0)))
  out01 <- do.call(px_fx, c(params, list(px = 0.5, max_cond_at = 0.1)))

  # Required outputs present
  expect_true("psi.px" %in% names(out0))
  expect_true("max_c"  %in% names(out0))
  expect_true("psi.px" %in% names(out01))
  expect_true("max_c"  %in% names(out01))

  # Core diagnostic: at least one of these should change when max_cond_at changes
  expect_true(
    (out0$psi.px != out01$psi.px) || (out0$max_c != out01$max_c),
    info = paste(
      "Changing max_cond_at from 0 to 0.1 did not change psi.px or max_c."
    )
  )
})


test_that("estimate_pxs passes max_cond_at correctly (p50_atmaxcond equals direct px_fx at px=0.5, max_cond_at=0.1)", {
  px_fx <- hydrafit::psiPx("sig")
  params <- list(A = 1, B = 2, C = 3)

  direct <- do.call(px_fx, c(params, list(px = 0.5, max_cond_at = 0.1)))$psi.px

  out <- estimate_pxs(
    params = params,
    px_fx = px_fx,
    px = c(0.20, 0.50, 0.80, 0.95),
    max_cond_at = 0.1
  )

  expect_equal(out$p50_atmaxcond, direct)
})
