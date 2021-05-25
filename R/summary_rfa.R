#' Return Summary Statistics for `rfa` object.
#'
#' The function returns the estimated coefficient, standard error,
#' t-statistic, p-value, confidence intervals, and random forest fit
#' performance for a `rfa` model object.
#'
#' @param model a `rfa` model object
#'
#' @return Returns summary statistics for the ATE obtained via the
#' RFA routine.
#'
#' @seealso The `rfa` function.
#'
#' @export
summary_rfa <- function(model){

  # Estimate SE, t-stat, and p-val
  out <- tidy(model$fit) %>%
    select(term, estimate, std.error, statistic, p.value, conf.low, conf.high) %>%
    mutate(
      OOB.response = model$yrf$rsquared,
      OOB.predictor = model$xrf$rsquared
    )

  # Return output
  return(out)
}
