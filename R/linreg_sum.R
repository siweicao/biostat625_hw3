#' Linear Regression Summary
#'
#' Gives summary results of model along with addition statistics not included with basic `linreg()` function.
#'
#' @param formula a formula with the form 'response~covariate+covariate...'.
#' @param data a data frame containing the variables specified in 'formula'.
#'
#' @return returns a list with the following elements:
#' \item{call}{the function that was called}
#' \item{sum_residuals}{summary statistics of the residuals including minimum, 1st quartile, median, mean, 3rd quartile, and maximum}
#' \item{coefficients}{the coefficients estimates, standard error, t-statistic value, and p-value for each predictior variable specified in model}
#' \item{sigma}{residual standard error}
#' \item{r.squared}{R squared value}
#' \item{adj.r.squared}{adjusted R square value}
#' \item{fstatistic}{f-statistic value, degrees of freedom for numerator, and degrees of freedom for denominator}
#' \item{f.stat.pvalue}{f-statistic p-value}
#' \item{residuals}{a vector of residuals, that is response minus fitted values, with each residual specified by observation}
#' \item{df}{degrees of freedom with the first being number of non-aliased coefficients, degrees of freedom for model, and total number of coefficients}
#' \item{terms}{the terms objects that were used in model}
#'
#' @export
#'
#' @examples
#' linreg_sum(Temp~Wind+Month+Ozone,airquality)
#'
#'
#'


linreg_sum = function(formula, data) {
  model = linreg(formula, data) #
  X = model.matrix(formula)
  Y = as.matrix(model.frame(formula)[1])
  n = nrow(X)
  p = ncol(X)
  betahat = solve(t(X)%*%X)%*%t(X)%*%Y
  Yhat = X%*%betahat
  epsilonhat = Y-Yhat
  sigmasquared = t(epsilonhat)%*%epsilonhat/(n-p)
  sigma = as.numeric(sqrt(sigmasquared))
  var_betahat = diag(solve(t(X)%*%X))*c(sigmasquared)
  se_betahat = sqrt(var_betahat)
  t_stat = c(betahat/se_betahat)
  pvalue = c(2*(1-pt(q=abs(t_stat), df=n-p)))
  Ybar = mean(Y)
  SSE = sum((Y-Yhat)^2)
  SSR = sum((Yhat-Ybar)^2)
  MSE = SSE/(n-p)
  MSR = SSR/(p-1)
  f_stat = MSR/MSE
  f_stat_vec = c(f_stat, p-1, n-p)
  f_statpv = 1-pf(q = f_stat, df1 = p-1, df2 = n-p)
  names(f_stat_vec) = c("value", "numdf", "dendf")
  df = c(p, n-p, p)
  r_squared = SSR/(SSR+SSE)
  #creates matrix of
  coefficients = cbind(Estimate = c(betahat), "Std. Error" = se_betahat, "t value" = t_stat,
                       "Pr(>|t|)" = pvalue)

  sum_list = list(call = match.call(linreg_sum), sum_residuals = summary(model$residuals), coefficients =
                    coefficients, sigma = sigma, r.squared = r_squared, adj.r.squared =
                    1-((SSE/(n-p))/((SSR+SSE)/(n-1))), fstatistic = f_stat_vec, f.stat.pvalue = f_statpv,
                  residuals = model$residuals, df = df, terms = model$terms)
  class(sum_list) = 'summary.linreg'

  return(sum_list)
}
