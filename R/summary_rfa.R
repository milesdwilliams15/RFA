#' Return Summary Statistics for ATE Estimated with `rfa`
#'
#' The function returns the estimated ATE, standard error,
#' t-statistic, and p-value for a `rfa` model object.
#'
#' @param model a `rfa` model object
#'
#' @details The function takes the estimated ATE and bootstrapped estimates
#' returned by an `rfa` object to calculate the summary statistics for the
#' ATE calculated from the RFA routine.
#'
#' @return Returns summary statistics for the ATE obtained via the
#' RFA routine.
#'
#' @seealso The `rfa` function.
#'
#' @export
summary_rfa = function(model){

  # Estimate SE, t-stat, and p-val
  estimate = model$ate
  std.error = sd(model$bootate)
  statistic = estimate/std.error
  p.value = pnorm(statistic, lower.tail = FALSE)

  # Summarize output
  out = data.frame(estimate, std.error, statistic, p.value)
  out = round(out, 4)
  stars = function(x){
    if(x <= 0.001) {
      return("***")
    } else if (x <= 0.01) {
      return("**")
    } else if (x <= 0.05) {
      return("*")
    } else if (x <= 0.1) {
      return(".")
    } else {
      return(" ")
    }
  }
  out$x = stars(out$p.value)
  out = dplyr::rename(out, ` ` = x)
  rownames(out) = "ATE"

  # Return output
  cat("RESULTS FROM RANDOM FOREST RESIDUALIZATION ANALYSIS\n")
  cat("\n")
  cat("Estimate Summary:\n")
  print(out)
  cat("---\n")
  cat("Signif. codes: 0.001 '***' 0.01 '**' 0.05 '*' 0.1 '.' 1 ' ' ")
}
