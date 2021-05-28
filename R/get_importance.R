#' Collect Variable Importance Metrics for Covariates Used in Random Forest Adjustment
#'
#' This function returns variable importance metrics (either "impurity" or "permutation") for each of the covariates
#' included in random forest adjustment estimation with respect to the response
#' variable and the explanatory variable of interest.
#'
#' @param rfa an `rfa()` fitted object
#'
#' @details To return importance metrics, in `rfa()` the argument `importance` must be set to either "impurity" or "permutation" (quoted).
#'
#' @export
get_importance <-
  function(
    rfa
  ) {
    # return error if importance wasn't specified by user in rfa()
    if(rfa$yrf$importance.mode == "none") {
      stop("Importance was not specified in rfa(). Please specify either:\n  importance = 'impurity'  OR  importance = 'permutation'.")
    }

    # return message specifying the type of importance metric being returned
    message(paste0("Returning importance type: ", rfa$yrf$importance.mode))

    # get variable importance for response
    yimp <- rfa$yrf$variable.importance

    # get variable importance for explanatory variable
    ximp <- rfa$xrf$variable.importance

    # get covariate names
    varnames <- colnames(rfa$covmat)

    # make data frame to return
    imptab <- data.frame(
      term = varnames,
      response = yimp,
      predictor = ximp,
      row.names = NULL
    )
    return(imptab)
  }
