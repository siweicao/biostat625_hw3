linreg=function(formula,data) {
  attach(data)
  X=model.matrix(formula)
  Y=as.matrix(model.frame(formula)[1])
  n=nrow(X)
  p=ncol(X)
  betahat=solve(t(X)%*%X)%*%t(X)%*%Y
  Yhat=X%*%betahat
  call=match.call(linreg)
  coefficients=c(betahat)
  names(coefficients)=row.names(betahat)
  residuals=t(Y-Yhat)
  names=colnames(residuals)
  residuals=as.vector(residuals)
  names(residuals)=names
  fitted.values=t(Yhat)
  names=colnames(fitted.values)
  fitted.values=as.vector(fitted.values)
  names(fitted.values)=names

  linreg_list=list(call=call,coefficients=coefficients,residuals=residuals,rank=p,fitted.values=fitted.values,
                   df.residual=(n-p),terms=terms(formula))

  return (linreg_list)
}


