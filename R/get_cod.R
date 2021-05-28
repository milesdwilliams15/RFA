#' Evaluate the Effective Random Forest Adjusted Sample
#'
#' This function returns the "coefficient of distortion" (COD) which reflects the
#' exent to which the effective sample produced via random forest adjustment
#' differs from the nominal (raw) data sample prior to covariate adjustment
#' (see Aronow and Samii 2016).
#' The COD is the weighted mean of a given de-meaned and standardized covariate
#' used in covariate adjustment where the weights are equivalent to the squared
#' residual error for a given observation from the random forest regression
#' used to partial out variation in the explanatory variable of interest given
#' the set of covariates adjusted for via random forest adjustment. The COD
#' thus represents the difference in standard deviation units between the effective
#' sample used to identify the relationship between the explanatory variable of
#' interest and the response, and the nominal sample used prior to estimation.
#'
#' The function accepts an `rfa` fitted object, and returns a tibble containing
#' the COD for each covariate included in estimation, and its standard error.
#' Standard errors are produced via bootstrapping.
#'
#' @param rfa an `rfa()` fitted object.
#'
#' @param include_se set to TRUE by default. If TRUE, will return standard errors with summary statistics for each of the estimated CODs.
#'
#' @param bootsims set to 1,000 by default. If `include_se = TRUE`, the number of bootstrap iterations to perform in estimating standard errors.
#'
#' @return The function returns a data frame containing at minimum a vector of covariate names (`term`) and the estimated COD per covariate (`estimate`). If `include_se = TRUE`, additional entries include the standard error, test statistic, p-value, and 95% confidence intervals.
#'
#' @references Aronow, Peter M. and Cyrus Samii. 2016. "Does Regression Produce Representative Estimates of Causal Effects?" American Journal of Political Science 60(1): 250-67.
#'
#' @export
get_cod <-
  function(
    rfa,
    include_se = TRUE,
    bootsims = 1000
  ) {
    # get the matrix of covariates
    covmat <- rfa$covmat

    # get the covariate names
    covnames <- colnames(covmat)

    # get the regression weights for the
    # covariate of interest
    wts <- rfa$data$xres^2

    # function to de-mean and standardize
    stand <- function(x) (x - mean(x)) / sd(x)

    # function to get weighted mean
    wmean <- function(x, wt = wts) sum(wt * stand(x)) / sum(wt)

    # compute COD for each variable in covmat
    cod <- apply(covmat, 2, wmean)

    # place in data frame
    codtab <- data.frame(term = covnames, estimate = cod,
                         row.names = NULL)

    # get standard errors via bootstrapping
    if(include_se == TRUE) {
      message("Bootstrapping....")
      bootcods <-
        replicate(
          n = bootsims,
          expr = {
            b <-
              sample(
                1:nrow(covmat),
                replace = TRUE,
                size = nrow(covmat)
              )
            apply(covmat[b, ], 2,
                  function(x) wmean(x, wt = wts[b]))
          }
        )
      codtab$std.error <-
        apply(bootcods, 1, sd)
      codtab$statistic <-
        codtab$estimate / codtab$std.error
      codtab$p.value <-
        2 * pnorm(-abs(codtab$statistic))
      codtab$conf.low <-
        codtab$estimate - 1.96 * codtab$std.error
      codtab$conf.high <-
        codtab$estimate + 1.96 * codtab$std.error
    }

    # return output
    return(codtab)
  }
