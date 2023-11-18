#' Linear Regression
#'
#' Fits univariate or multivariate linear regression models
#'
#' @param formula a formula with the form 'response~covariate+covariate...'.
#' @param data a data frame containing the variables specified in 'formula'.
#'
#' @return returns a list with the following elements:
#' \item{call}{the function that was called}
#' \item{coefficients}{the coefficients estimates for each predictior variable specified in model}
#' \item{residuals}{a vector of residuals, that is response minus fitted values, with each residual specified by observation}
#' \item{rank}{the numeric rank for the model}
#' \item{fitted.values}{the fitted values}
#' \item{df.residual}{the residual degrees of freedom, that is number of observations minus number of predictor variables}
#' \item{terms}{the terms objects that were used in model}
#'
#'@export
#'
#' @examples
#' linreg(Temp~Wind+Month+Ozone,airquality)
#'
#'

linreg = function(formula, data) {
  #creates design matrix from formula
  X = model.matrix(formula)
  #subsets formula for response variable
  Y = as.matrix(model.frame(formula)[1])
  n = nrow(X)
  p = ncol(X)
  #solves to find coefficient estimates
  betahat = solve(t(X)%*%X)%*%t(X)%*%Y
  Yhat = X%*%betahat
  call = match.call(linreg)
  coefficients = c(betahat)
  names(coefficients) = row.names(betahat)
  residuals = t(Y-Yhat)
  names = colnames(residuals)
  residuals = as.vector(residuals)
  names(residuals) = names
  fitted.values = t(Yhat)
  names = colnames(fitted.values)
  fitted.values = as.vector(fitted.values)
  names(fitted.values) = names
  linreg_list = list(call = call, coefficients = coefficients, residuals = residuals, rank = p, fitted.values
                     = fitted.values, df.residual = (n-p), terms = terms(formula))
  class(linreg_list) = 'linreg'
  return (linreg_list)
}



