#' Wrapper for ggplot2 to Plot Histogram of Empirical ATE Replicates
#'
#' This function takes output from a `rfa` object and returns a
#' histogram of the bootstrapped ATE replicates.
#'
#' @export
plot_rfa = function(model){
  ggplot2::qplot(model$bootate,
                 xlab = "Empirical Distribution of ATE\n(Vertical Line Denotes Observed Estimate)",
                 ylab = "Frequency\n(out of 999 replicates)") +
    ggplot2::geom_vline(xintercept = model$ate)
}
