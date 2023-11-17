# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'

hello <- function() {
  print("Hello, world!")
}


linreg=function(formula,data) {
  attach(data)
  X=model.matrix(formula)
  Y=as.matrix(model.frame(formula)[1])
  n=nrow(X)
  p=ncol(X)
  betahat=solve(t(X)%*%X)%*%t(X)%*%Y
  Yhat=X%*%betahat
  epsilonhat=Y-Yhat
  sigmasquared=t(epsilonhat)%*%epsilonhat/(n-p)
  var_betahat=diag(solve(t(X)%*%X))*c(sigmasquared)
  se_betahat=sqrt(var_betahat)
  t_stat=c(betahat/se_betahat)
  pvalue=c(2*(1-pt(q=abs(t_stat),df=n-p)))

  by_hand=list()
  by_hand$coefficients=cbind(Estimate=c(betahat), "Std. Error"=se_betahat, "t value"=t_stat, "Pr(>|t|)"=pvalue)
  by_hand$residuals=as.numeric(epsilonhat)


  return (by_hand)
}

linreg(avg_sleep~age,data)

typeof(linreg(avg_sleep~age,data)$residuals)

linreg(avg_sleep~age,data)$residuals
