#' @title A function that creates a data frame from an object of class linreg
#' @description A function that creates a data frame from an object of class linreg
#' @param x linreg object
#' @param ... other arguments
#' @return data.frame contains fitted values and residuals.
as.data.frame.linreg <- function(x, ...) {
  datafr <- data.frame(fitted=x$fitted_values, residuals=x$resi)
  return(datafr)
}



#' @title Method for printing the coefficients and coefficient names
#' @description Method for printing the coefficients and coefficient names
#' @param x linreg object
#' @param ... other arguments
print.linreg <- function(x,...){
  data_name <- x$data_name
  formula_str = Reduce(paste, deparse(x$formula))
  cat("Call:\n")
  cat(paste("linreg(formula = ",formula_str,", data = ", data_name,")\n\n",sep=""))
  cat("Coefficients:\n")
  print(x$reg_coef)
}



#' @title The plot-method that creates two plots
#' @description The plot-method that creates two plots
#' @import gridExtra
#' @param x linreg object
#' @param ... other arguments
#' @return ggplot2 figures
plot.linreg <- function(x, ...) {
  form_temp <- as.character(x$formula)
  form <- paste("linreg(",form_temp[2],form_temp[1],form_temp[3],")")
  z <- as.data.frame(x)
  p1<-ggplot(z, aes(x=fitted, y=residuals)) +
    geom_point(shape=1, size=5) +
    xlab(paste("Fitted values",form, sep="\n")) +
    ylab("Residuals") +
    ggtitle("Residuals vs Fitted") +
    geom_text(aes(label = tail(z$residuals,1), x=max(z$fitted), y=max(z$residuals)), hjust=1.5, size = 3)+
    stat_summary(aes(x=fitted,group=1),fun.y=median, colour="red", geom="line")+
    stat_summary(aes(x=fitted,group=1),fun.y=mean, colour="gray", geom="line",linetype=2)

  mod_residuals <- sqrt(abs(z$residuals / sqrt(x$res_var)))
  z[,3]<-mod_residuals
  colnames(z)[3] <- "mod_residuals"
  p2<-ggplot(z[,c(1,3)], aes(x=fitted, y=mod_residuals)) +
    geom_point(shape=1, size=5) +
    xlab(paste("Fitted values",form, sep="\n")) +
    ylab("sqrt(|Standardized residuals|)") +
    ggtitle("Scale-Location")+
    stat_summary(aes(x=fitted,group=1),fun.y=median, colour="red", geom="line")
  return(grid.arrange(p1,p2,ncol=1,newpage=F))}



#' @title A method that returns the vector of residuals
#' @description A function that check the residulas from an object of class linreg
#' @param x linreg object.
#' @param ... other arguments.
#' @return vector of residuals.
resid<-function(x,...)UseMethod("resid")
resid.linreg <- function(x){return(x$resi)
}



#' @title A method that returns the predicted values y.
#' @description A function that return the predicted values y.
#' @param x linreg object
#' @param ... other arguments
#' @return vector of predictions.
pred<-function(x,...)UseMethod("pred")
pred.linreg <- function(x){
  return(x$fitted_values)
}



#' @title A method that returns the coefficients as a named vector
#' @description A method that returns the coefficients as a named vector
#' @param object linreg object
#' @param ... other arguments
#' @return named vector of coefficients.
coef.linreg <- function(object, ...){
  return(object$reg_coef)
}



#' @title A method that creates a summary of the linreg-function
#' @description A method that creates a summary of the linreg-function
#' @param object linreg object
#' @param ... other arguments
#' @return linreg_summary object, can be printed.
summary.linreg <- function(object, ...){
  tmp = c(as.matrix(object$reg_coef),as.matrix(object$var_reg_coef),object$t_each_coef,object$p_values)
  tmp = matrix(tmp,nrow=length(object$reg_coef))
  tmp = cbind(tmp,1:length(object$reg_coef))
  tmp = as.data.frame(tmp)
  p =object$p_values
  for(i in 1:length(object$reg_coef)){
    if(p[i]>=0&&p[i]<0.001){
      tmp[i,5]<-"***"
    }else if(
      p[i]>=0.001&&p[i]<0.01){
      tmp[i,5]<-"**"
    }else if(
      p[i]>=0.01&&p[i]<0.05){
      tmp[i,5]<-"*"
    }else if(
      p[i]>=0.05&&p[i]<0.1){
      tmp[i,5]<-"."
    }else if(
      p[i]>=0.1&&p[i]<1){
      tmp[i,5]<-""
    }
    tmp
  }

  rownames(tmp) = names(object$reg_coef)
  colnames(tmp) = c("Estimate", "Std. Error", "t value", "Pr(>|t|)","")

  summary = list()
  summary$coefficients = tmp
  summary$df = object$deg_free
  summary$rse = round(sqrt(object$res_var),4)
  summary$formula = Reduce(paste, deparse(object$formula))
  summary$data_name=object$data_name
  class(summary) <- 'linreg_summary'
  return(summary)
}



#' @title Method for printing a summary of the results from the linreg function
#' @description Method for printing a summary of the results from the linreg function
#' @param x linreg object
#' @param ... other arguments
print.linreg_summary <- function(x, ...){

  cat("Call:\n")
  cat(paste("linreg(formula = ",x$formula,", data = ", x$data_name,")\n\n",sep=""))

  cat('Coefficients: \n')
  print(x$coefficients)
  cat('--- \n')
  cat("Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 \n\n")

  cat(paste(
    'Residual standard error:',
    round(x$rse,4),
    'on',
    x$df,
    'degrees of freedom'
  ))
  cat('\n')
}
