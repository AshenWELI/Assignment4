#' Multiple Linear Regression
#'
#' Fits a multiple linear regression model.
#'
#' @param formula a formula specifying the regression model, e.g., y ~ x1 + x2
#' @param data a data frame containing the variables specified in the formula
#' @return An object of class "linreg" containing regression results
#' @export
linreg<-function(formula,data){
  mat<-model.matrix(formula,data)
  y<-data[[all.vars(formula)[1]]]
  
  beta_hat<-solve(t(mat)%*%mat)%*%t(mat)%*%y # calculating OLS coefficients
  
  y_hat<-mat%*%beta_hat   #the fitted value
  residuals<- y-y_hat     #The residuals
  n<-length(y)            
  p<-ncol(mat)
  df<-n - p               #The degrees of freedom
  residual_variance<-sum(residuals^2)/df #The residual variance
  
  var_beta_hat <- residual_variance * solve(t(mat) %*% mat)  #The variance of the regression coefficients
  
  t_values <- beta_hat/ sqrt(diag(var_beta_hat))   #t value
  p_values <- 2 * (1 - pt(abs(t_values), df))       #p value
  
  final_result<-list(
    coefficients = beta_hat,
    fitted.values = y_hat,
    residuals = residuals,
    df = df,
    residual.variance = residual_variance,
    var_coefficients = var_beta_hat,
    t_values = t_values,
    p_values = p_values
  )
  
  class(final_result)<-"linreg"
  return(final_result)
}
