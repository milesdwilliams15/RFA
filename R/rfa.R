#' Estimate Average Treatment Effect via Random Forest Adjustment
#'
#' This function estimates the average treatment effect of an explanatory
#' variable on some response variable using a procedure called Random Forest
#' Adjustment (RFA). RFA partials out the variation in a response and explanatory
#' variable of interest as a function of a set of covariates using random forest
#' regression. The latest version relies on the `{ranger}` package, which
#' provides a fast implementation of the random forest aglorithm (Brieman 2001).
#'
#' The package further supports fixed and random effects as well. For random effects,
#' the `lmer` function from the `{lme4}` package is used. By specifying fixed or 
#' random effects, the response, treatment, and covariates are demeaned according to
#' any fixed or random effects specified prior to random forest adjustment.
#'
#' @references Breiman, Leo. 2001. "Random Forests." Machine Learning 45: 5-32.
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
#' @param fes_and_res a formula object only containing the righ-hand side specifying 
#' any fixed effects or random effects. If random effects, you should use the notation
#' `~ (1 | id)` as in the `{lme4}` package.
#' @param data an optional data frame containing the variables used to
#' implement the RFA routine.
#' @param se_type specifies the standard errors to be returned. If `clusters` is not
#' specified, the user can specify "classical", "HC0", "stata" (equivalent to "HC1"),
#' "HC2", or "HC3". If `clusters` is specified, the options are "CR0", "stata" (CR1),
#' and "CR2". "stata" is the default.
#' @param clusters optional name (quoted) of variable that corresponds to clusters
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
#' the `estimatr`` package), the data used to estimate the the model, a covariates matrix, and the
#' random forest regressions for the response and explanatory variable
#' (`ranger` objects from the `ranger` package).
#' @export
rfa <- function(
  formula,
  covariates,
  fes_and_res = NULL,
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
  rhs2 <- unlist(strsplit(deparse(covariates[[2]]), " \\+ "))
  if(!is.null(fes_and_res)) {
    fes  <- unlist(strsplit(deparse(fes_and_res[[2]]), " \\+ "))
    any_res <- str_detect(fes, "\\|")
    rhs <- c(rhs1, rhs2, fes)
  } else {
    rhs <- c(rhs1, rhs2)
    any_res <- FALSE
  }
  if(!is.null(clusters)) rhs <- c(rhs, clusters)
  fullform <- reformulate(rhs, lhs)
  if(any_res) {
    data <- lmer(fullform, data = data) |> model.frame()
  } else {
    data <- model.frame(fullform, data)
  }

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

  # Demean the response, treatment, and covariates for any FEs and REs
  if(!is.null(fes_and_res)) {
    if(any_res) {
      for(i in 1:ncol(covmat)) {
        covmat[, i] <- resid(
          lmer(update(fes_and_res, covmat[, i] ~ .), data = data)
        )
      }
      for(i in 1:ncol(varmat)) {
        varmat[, i] <- resid(
          lmer(update(fes_and_res, covmat[, i] ~ .), data = data)
        )
      }
    } else {
      for(i in 1:ncol(covmat)) {
        covmat[, i] <- resid(
          lm(update(fes_and_res, covmat[, i] ~ .), data = data)
        )
      }
      for(i in 1:ncol(varmat)) {
        varmat[, i] <- resid(
          lm(update(fes_and_res, covmat[, i] ~ .), data = data)
        )
      }
    }
  }

  # Adjust for covaraites using random forests
  ## The response
  yrf <-
    ranger(
      y = varmat[, 1],
      x = cbind(covmat),
      ...
    )
  yhat <- yrf$predictions
  if(!is.null(dim(yhat))) yhat <- yhat[, 2]
  yres <- varmat[, 1] - yhat

  ## The treatment
  xrf <-
    ranger(
      y = varmat[, 2],
      x = cbind(covmat),
      ...
    )
  xhat <- xrf$predictions
  if(!is.null(dim(xhat))) xhat <- xhat[, 2]
  xres <- varmat[, 2] - xhat

  # Estimate the marginal effect
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
    covmat = covmat,
    data = data
  )
  return(lst)
}
