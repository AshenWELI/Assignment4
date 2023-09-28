
linreg <- function(formula, data) {
  #install.packages("plyr")
  #library(plyr)
  if ( !plyr::is.formula(formula) | !is.data.frame(data)) {
    stop("Your arugments should be a formula and a dataframe dataset, please check!!")
  }
  dataset_name <- deparse(substitute(data))
  #create matrix X with independent variables
  X <- model.matrix(formula, data)
  #create dependent varabile y
  all_vars<-all.vars(formula)
  y <- data[,all_vars[1]]
  
  #calculate X'X and X'y
  XtX <- t(X) %*% X
  Xty <- t(X) %*% y
  
  #calculate the coefficients β^ using the formula
  beta_hat <- solve(XtX) %*% Xty

  #calculate the fitted values
  fitted_values <- X %*% beta_hat
    
  #calculate the residuals 
  residuals <- y - X %*% beta_hat
  
  #calculate the degrees of freedom
  # n is the number of observations
  # p is number of predictor variables
  n <- length(y)
  p <- ncol(X) 
  df <- n - p
  
  #calculate the residual variance
  residul_variance <- sum(residuals^2) / df
  
  #calculate the variance of the regression coefficients
  variance_beta_hat <- residul_variance * solve(t(X) %*% X)
  
  #calculate standard errors of coeficients
  sd_errors <- sqrt(diag(variance_beta_hat))
  
  #calculate t-values for each coefficient
  t_values <- beta_hat / (sqrt(diag(variance_beta_hat)))
  
  #calculate p-values for each coefficient
  p_values <- 2*pt(-abs(t_values), df=length(data)-1)
  
  
  #define an RC class object named linreg
  linreg <- setRefClass("linreg",fields=list(Formula="formula",
                                             Dataset="data.frame",
                                             DatasetName="character",
                                             RegressionsCoefficients="matrix",
                                             FittedValues="matrix",
                                             Residuals="matrix",
                                             DegreesofFreedom="numeric",
                                             ResidulVariance="numeric",
                                             VarianceofCoefficients="matrix",
                                             StandardErrors="numeric",
                                             TValues="matrix",
                                             PValues="matrix"),
                        methods = list(
                          # print function for linreg
                          print = function(){
                            cat ("call:\n")
                            cat (paste0("linreg(formula = ", deparse(Formula), ", data = ", DatasetName, ")\n"))
                            cat ("\nCoefficients:\n")
                            base::print(RegressionsCoefficients[,1])
                          },
                          # plot() function for linreg
                          plot = function(){
                            #create first plot's specific points dataframe
                            g1_specific_points_df <- data.frame(
                              Residuals = c(Residuals[99], Residuals[118], Residuals[119]),
                              FittedValues = c(FittedValues[99], FittedValues[118], FittedValues[119]),
                              Label = c("99", "118", "119")
                            )
                            
                            #create first plot for residuals VS fitted Values 
                            g1 <- ggplot2::ggplot(Dataset, aes(x = FittedValues, y = Residuals)) + 
                              geom_point(shape = 1) + 
                              labs(x = paste0("Fitted Values\n linreg(", deparse(Formula), ")"), y = "Residuals", title = "Residuals VS Fitted") + 
                              stat_summary(fun = median, geom = "line", color="red") + 
                              theme_test() + 
                              theme(plot.title = element_text(size=12,hjust=0.5)) + 
                              geom_hline(yintercept = 0, color = "grey", linetype = "dotted") + 
                              geom_text(data = specific_points_df, aes(x = FittedValues, y = Residuals, label = Label), hjust = -0.2, vjust = 0.5, size = 3)
                            
                            #create second plot's dataframe from the linreg mode
                            g2_residuals_fitted_df <- data.frame(
                              Sqrt_Abs_Residuals = sqrt(abs(Residuals/sd(Residuals))),
                              FittedValues = FittedValues
                            )
                            #create second plot's specific points dataframe
                            g2_specific_points_df <- data.frame(
                              Sqrt_Abs_Residuals = c(g2_residuals_fitted_df$Sqrt_Abs_Residuals[99], g2_residuals_fitted_df$Sqrt_Abs_Residuals[118], g2_residuals_fitted_df$Sqrt_Abs_Residuals[119]),
                              FittedValues = c(g2_residuals_fitted_df$FittedValues[99], g2_residuals_fitted_df$FittedValues[118], g2_residuals_fitted_df$FittedValues[119]),
                              Label = c("99", "118", "119")
                            )
                            
                            #create second plot for scale-location
                            g2 <- ggplot2::ggplot(g2_residuals_fitted_df, aes(x = FittedValues, y = Sqrt_Abs_Residuals)) + 
                              geom_point(shape = 1) + 
                              labs(x = paste0("Fitted Values\n linreg(", deparse(Formula), ")"), y = expression(sqrt(abs("Standardized residuals"))), title = "Scale-Location") + 
                              stat_summary(fun = median, geom = "line", color="red") + 
                              theme_test() + 
                              theme(plot.title = element_text(size=12,hjust=0.5)) + 
                              geom_text(data = g2_specific_points_df, aes(x = FittedValues, y = Sqrt_Abs_Residuals, label = Label), hjust = -0.2, vjust = 0.5, size = 3)  
                            
                            plots <- list(g1,g2)
                            for (i in 1:length(plots)) {
                              base::plot(plots[[i]])
                              #print a message to ask user to press Enter
                              cat("Hit <Return> to see the next plot...")
                              #wait for user to press Enter
                              readline(prompt = "")
                            }
                          },
                          # resid() function for linreg
                          resid = function(){
                            base::print(Residuals[,1])
                          },
                          # pred() function for linreg
                          pred = function(){
                            base::print(FittedValues[,1])
                          },
                          # coef() function for linreg
                          coef = function() {
                            base::print(RegressionsCoefficients[,1])
                          },
                          # summary() function for linreg
                          summary = function() {
                            cat ("call:\n")
                            cat (paste0("linreg(formula = ", deparse(Formula), ", data = ", DatasetName, ")\n"))
                            cat ("\nResiduals:\n")
                            # Calculate the Residuals statistics
                            min_residual <- min(Residuals)
                            q1_residual <- quantile(Residuals, 0.25)
                            median_residual <- median(Residuals)
                            q3_residual <- quantile(Residuals, 0.75)
                            max_residual <- max(Residuals)
                            resid_matrix <- matrix(0, nrow = 1, ncol = 5)
                            colnames(resid_matrix) <- c("Min", "1Q", "Median", "3Q", "Max")
                            resid_matrix[1, ] <- c(min_residual, q1_residual, median_residual, q3_residual, max_residual)
                            base::print(resid_matrix, quote = FALSE)
                            cat ("\nCoefficients:\n")
                            coef_matrix <- cbind(RegressionsCoefficients, StandardErrors, TValues, PValues)
                            colnames(coef_matrix) <- c("Estimate", "Std. Error", "t value", "Pr(>|t|)")
                            base::print(coef_matrix)
                            cat (paste0("\nDegrees of freedom is: ", DegreesofFreedom))
                          }
                        ))
  #create an instance of linreg to store the results
  linreg_obj <- new("linreg",
                    Formula = formula,
                    Dataset = data,
                    DatasetName = dataset_name,
                    RegressionsCoefficients = beta_hat,
                    FittedValues = fitted_values,
                    Residuals = residuals,
                    DegreesofFreedom = df,
                    ResidulVariance = residul_variance,
                    VarianceofCoefficients = variance_beta_hat,
                    StandardErrors = sd_errors,
                    TValues = t_values,
                    PValues = p_values
    
  )
  return(linreg_obj)
}

