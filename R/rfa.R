#' Estimate Average Treatment Effect via Random Forest Adjustment
#'
#' This function estimates the average treatment effect of an explanatory
#' variable on some response variable using a procedure called Random Forest
#' Adjustment (RFA). RFA partials out the variation in a response and explanatory
#' variable of interest as a function of a set of covariates using random forest
#' regression. The latest version relies on the ranger package, which
#' provides a fast implementation of the random forest aglorithm (Brieman 2001).
#'
#' The function accepts a formula object and dataframe
#' as inputs. rfa() assumes that the first right-hand side variable in the
#' formula object is the explanatory variable of interest and that all other
#' variables on the right-hand side are confounding variables used to
#' residualize the predictor and response prior to estimating the ATE. NA
#' values are allowed.
#'
#' @param formula a formula object where the left-hand variable is the outcome
#' and the right-hand variable is the explanatory variable of interest.
#' @param covariates a formula object only containing the right-hand side specifying
#' the covariates to be used in the random forest regressions.
#' @param data an optional data frame containing the variables used to
#' implement the RFA routine.
#' @param se_type specifies the standard errors to be returned. If `clusters` is not
#' specified, the user can specify "classical", "HC0", "stata" (equivalent to "HC1"),
#' "HC2", or "HC3". If `clusters` is specified, the options are "CR0", "stata" (CR1),
#' and "CR2". "stata" is the default.
#' @param clusters optional name (unquoted) of variable that corresponds to clusters
#' in the data.
#' @param ... additional commands to override the default settings for implementing
#' random forests via `ranger`. See the `ranger` package for more details.
#' @details The RFA routine provides an estimate of the marginal relationship
#' between some
#' predictor (either binary or continuous) and a response variable, adjusting
#' for the confounding influence of other variables. As its name implies,
#' RFA substracts away the variance in the predictor of interest and response explained
#' by confounding variables via random forest regression.
#' @return `rfa` returns a list containing the model object (`lm_robust` object from
#' the `estimatr`` package), the data used to estimate the the model, and the
#' random forest regressions for the response and explanatory variable
#' (`ranger` objects from the `ranger` package).
#' @export
rfa <- function(
  formula,
  covariates,
  data = NULL,
  se_type = "stata",
  clusters = NULL,
  ...
) {

  # Return error if covariates includes lhs variable
  if(formula[[2]] == covariates[[2]])
    stop("Covariates formula should be specified without left-hand side variable.")

  # Return error if se_type is not valid
  se_types <- c("classical","HC0","stata","HC2","HC3","CR0","CR2")
  if(!any(se_type==se_types))
    stop("se_type must be one of: ", paste(se_types, collapse = ", "))

  # Get the full data frame
  lhs <- deparse(formula[[2]])
  rhs1 <- strsplit(deparse(formula[[3]]), " \\+ ")[[1]]
  rhs2 <- strsplit(deparse(covariates[[2]]), " \\+ ")[[1]]
  rhs <- c(rhs1, rhs2)
  fullform <- reformulate(rhs, lhs)
  data <-
    model.frame(
      fullform, data = data
    )

  # Get design matrix of covariates
  covmat <-
    model.matrix(
      covariates, data = data
    )[, -1]

  # Vectors of treatment and response
  varmat <-
    cbind(
      model.frame(
        formula, data = data
      )[, 1],
      model.matrix(
        formula, data = data
      )[, -1]
    )

  # Residulalize the response
  yrf <-
    ranger(
      y = varmat[, 1],
      x = cbind(covmat),
      ...
    )
  yhat <- yrf$predictions
  if(!is.null(dim(yhat))) yhat <- yhat[, 2]
  yres <- varmat[, 1] - yhat

  # Residualize the treatment
  xrf <-
    ranger(
      y = varmat[, 2],
      x = cbind(covmat),
      ...
    )
  xhat <- xrf$predictions
  if(!is.null(dim(xhat))) xhat <- xhat[, 2]
  xres <- varmat[, 2] - xhat

  # Estimate
  data <-
    data %>%
    mutate(yres = yres, xres = xres)
  if(!is.null(clusters)) clusters <- data[, clusters]
  fit <-
    lm_robust(
      yres ~ xres,
      data = data,
      se_type = se_type,
      clusters = clusters
    )

  # Return fitted model
  lst <- list(
    fit = fit,
    yrf = yrf,
    xrf = xrf,
    data = data
  )
  return(lst)
}
