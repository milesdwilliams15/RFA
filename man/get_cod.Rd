% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_cod.R
\name{get_cod}
\alias{get_cod}
\title{Evaluate the Effective Random Forest Adjusted Sample}
\usage{
get_cod(rfa, include_se = TRUE, bootsims = 1000)
}
\arguments{
\item{rfa}{an `rfa()` fitted object.}

\item{include_se}{set to TRUE by default. If TRUE, will return standard errors with summary statistics for each of the estimated CODs.}

\item{bootsims}{set to 1,000 by default. If `include_se = TRUE`, the number of bootstrap iterations to perform in estimating standard errors.}
}
\value{
The function returns a data frame containing at minimum a vector of covariate names (`term`) and the estimated COD per covariate (`estimate`). If `include_se = TRUE`, additional entries include the standard error, test statistic, p-value, and 95% confidence intervals.
}
\description{
This function returns the "coefficient of distortion" (COD) which reflects the
exent to which the effective sample produced via random forest adjustment
differs from the nominal (raw) data sample prior to covariate adjustment
(see Aronow and Samii 2016).
The COD is the weighted mean of a given de-meaned and standardized covariate
used in covariate adjustment where the weights are equivalent to the squared
residual error for a given observation from the random forest regression
used to partial out variation in the explanatory variable of interest given
the set of covariates adjusted for via random forest adjustment. The COD
thus represents the difference in standard deviation units between the effective
sample used to identify the relationship between the explanatory variable of
interest and the response, and the nominal sample used prior to estimation.
}
\details{
The function accepts an `rfa` fitted object, and returns a tibble containing
the COD for each covariate included in estimation, and its standard error.
Standard errors are produced via bootstrapping.
}
\references{
Aronow, Peter M. and Cyrus Samii. 2016. "Does Regression Produce Representative Estimates of Causal Effects?" American Journal of Political Science 60(1): 250-67.
}
