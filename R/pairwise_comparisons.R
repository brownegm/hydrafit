#' Pairwise Bootstrap Comparisons of Species Predictions
#'
#' @param bootstraps A list containing bootstrap predictions for different species based on best fitting model.
#' @param conf_level Confidence level for the confidence intervals (default is 0.95).
#' @importFrom tidyr pivot_wider
#' @importFrom dplyr select mutate summarise
#' @importFrom purrr map_dfr map
#' @import stats
#' @return A data frame with pairwise comparisons between species, including the proportion of times one species has a higher or lower prediction than another, confidence intervals for the differences, and p-values from a binomial test.
#' @export
#'
compare_sp_boot <- function(bootstraps,
                                           conf_level = 0.95) {

  # Check if the input is a boot_list
  #stopifnot(class(bootstraps) == "boot_list")
  #stopifnot(length(bootstraps) > 1 )

  # Get species names
  species_names <- sapply(seq_along(bootstraps),\(bb) bootstraps[[bb]]$species)

  # Create a data frame from the bootstraps
  bootstraps_df <- sapply(seq_along(bootstraps), \(bb) bootstraps[[bb]]$bootvals)|>
              as.data.frame()
  names(bootstraps_df) <- species_names

  # Identify all species combinations
  combos <- combn(species_names, 2, simplify = FALSE)

  # Loop through species pairs and compute comparison statistics
  results <- map_dfr(combos, function(pair) {
    sp_pair <- unlist(pair)
    pair_df <- bootstraps_df[sp_pair[c(1, 2)]]
    diffs <- pair_df[[1]] - pair_df[[2]]
    prop_gt <- mean(diffs > 0)
    prop_lt <- mean(diffs < 0)

    # determine which species is greater
    diff_stat <- if(prop_gt >= conf_level){diffs > 0} else {diffs < 0}

    # confidence intervals of the differences
    ci <- quantile(diffs,
                   probs = c((1 - conf_level)/2, 1 - (1 - conf_level)/2),
                   na.rm = TRUE)

    # perform exact binomial test on the probability of one species being greater than another
    binom_res <- binom.test(sum(diff_stat),
                            length(diffs),
                            p = 0.5,
                            alternative = "two.sided")

    list(
      species1 = pair[[1]],
      species2 = pair[[2]],
      prop_1_gt_2 = prop_gt,
      prop_1_lt_2 = prop_lt,
      ci_lower = ci[1],
      ci_upper = ci[2],
      p_value = binom_res$p.value,
      diffs = list(diffs)  # raw differences stored as a list-column
    )
  })

  return(results)
}
