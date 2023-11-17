linreg=function(formula,data) {
  attach(data)
  X=model.matrix(formula)
  Y=as.matrix(model.frame(formula)[1])
  n=nrow(X)
  p=ncol(X)
  betahat=solve(t(X)%*%X)%*%t(X)%*%Y
  Yhat=X%*%betahat
  epsilonhat=Y-Yhat
  call=call("linreg",formula=formula,data=quote(data))
  coefficients=c(betahat)
  names(coefficients)=row.names(betahat)
  residuals=as.vector(epsilonhat)
  names(residuals)=c(1:n)
  fitted.values=as.vector(Yhat)
  names(fitted.values)=c(1:n)

  linreg_list=list(call=call,coefficients=coefficients,residuals=residuals,rank=p,fitted.values=fitted.values,
                   df.residual=(n-p),terms=terms(formula))

  return (linreg_list)
}
