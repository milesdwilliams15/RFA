# RFA <img src="inst/logo.png" align="right" height="130" />

![R-version](https://img.shields.io/badge/R%20%3E%3D-3.4.3-brightgreen)
![updated](https://img.shields.io/badge/last%20update-05--28--2021-brightgreen)
![version](https://img.shields.io/badge/version-0.1.0.2-brightgreen)
![license](https://img.shields.io/badge/license-GPL--2-red)
![encoding](https://img.shields.io/badge/encoding-UTF--8-red)
[![orchid](https://img.shields.io/badge/ORCID-0000--0003--0192--5542-brightgreen)](https://orcid.org/0000-0003-0192-5542)


`RFA` is an R package for implementing random forest adjustment (RFA). RFA is a regression adjustment approach that partials out variation in a response and explanatory variable of interest given a set of covariates using random forests. The latest version of the package relies on `ranger`, which is a fast implementation of random forests.


# Installation
To install and attach the latest version of `RFA`, enter:

    devtools::install_github("milesdwilliams15/RFA")
    library(RFA)

# Usage
`RFA` relies on `ranger` to implement random forests, and `estimatr` to perform linear regression on the random forest adjusted explanatory variable and response. 

For a generic dataset, `dataset`, that contains vectors of some response variable `y`, an explanatory variable of interest `z`, and a set of confounding covariates `x1`, `x2`, and `x3`, random forest adjusted estimates are obtained by entering:

    rfa(
      y ~ z,
      covariates = ~ x1 + x2 + x3,
      data = dataset
    )

The function returns a list consisting of an `estimatr::lm_robust` object, the computed random forest regressions for the response and explanatory variable (`ranger::ranger` objects), and the dataset used to generate random forest adjusted estimates with the partialized versions of the response and explanatory variable appended.

For a more comprehensive summary of the approach and how to implement it in R, see the [RFA vignette](https://rpubs.com/milesdwilliams15/rfa-vignette).
