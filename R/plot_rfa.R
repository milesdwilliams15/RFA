#' Wrapper for ggplot2 to Plot Histogram of Empirical ATE Replicates
#'
#' This function takes output from a `rfa` object and returns a
#' histogram of the bootstrapped ATE replicates.
#'
#' @export
plot_rfa = function(model){
  tidy(model$fit) %>%
    ggplot() +
    aes(
      x = estimate,
      y = varnam,
      xmin = conf.low,
      xmax = conf.high
    ) +
    geom_point() +
    geom_errorbarh(height = 0)
}
