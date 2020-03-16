#' Estimate Average Treatment Effect via Random Forest Adjustment
#'
#' This function estimates the average treatment effect of an explanatory
#' variable on some response variable using a procedure called Random Forest
#' Adjustment (RFA). The function accepts a formula object and dataframe
#' as inputs. rfa() assumes that the first right-hand side variable in the
#' formula object is the explanatory variable of interest and that all other
#' variables on the right-hand side are confounding variables used to
#' residualize the predictor and response prior to estimating the ATE. NA
#' values are allowed.
#'
#' @param formula a formula object where the first right-hand side variable
#' is the predictor variable of interest (or the treatment variable) and
#' the remaining right-hand side predictors are confounding variables.
#' @param data an optional data frame containing the variables used to
#' implement the RFA routine.
#' @details The RFA routine provides an estimate of the ATE of some
#' predictor (either binary or continuous) on a response variable, adjusting
#' for the confounding influence of other variables. As its name implies,
#' RFA substracts away the variance in the treatment and response explained
#' by confounding variables via random forest regression, prior to
#' estimating the ATE of the treatment on the response.
#' @return `rfa` returns a list containing the estimated ATE
#' and a vector of bootstrapped ATE estimates (useful if the user wishes)
#' to visualize the empirical distribution of the ATE. It also returns
#' a data frame of the predictor variable and response variable, both
#' pre- and post- adjustment.
#' @export
rfa = function(formula, data = NULL){

  # Set up data
  d = model.frame(lm(formula, data = data))
  d1 = d[,-2] # dataframe that excludes the predictor of interest
  colnames(d1) = c('y',paste0('x',1:(ncol(d1)-1)))
  d1 = as.data.frame(d1)
  d2 = d[,-1] # dataframe that excludes the response variable
  colnames(d2) = c('z',paste0('x',1:(ncol(d2)-1)))
  d2 = as.data.frame(d2)

  # Predict response and predictor of interest
  yhat = suppressWarnings(predict(randomForest::randomForest(y ~ ., data = d1)))
  zhat = suppressWarnings(predict(randomForest::randomForest(z ~ ., data = d2)))

  # Residualize response and predictor of interest
  yres = d1$y - yhat
  zres = d2$z - zhat

  # Save response and predictor (pre- and post-)
  # to data frame
  data = data.frame(preY = d1$y,
                    preZ = d1$z,
                    postY = yres,
                    postZ = zres)

  # Estimate ATE
  atem = lm(yres ~ zres)
  estimate = coef(atem)[2]

  # Bootstrap for inference
  ates = 0
  for(i in 1:999){
    obs = sample(1:nrow(d), size = nrow(d), replace = T)
    newyres = fitted(atem) + resid(atem)[obs]
    newatem = lm(newyres ~ zres)
    ates[i] = coef(newatem)[2]
  }

  return(
    list(
      ate = estimate,
      bootate = ates,
      data = data
    )
  )
}
