#' @title boxlinre: A linear regression package in R
#' @description A linear regression package in R
#' @author Maintainer: Boxi Zhang bossyzhang@gmail.com
#' @references \url{https://en.wikipedia.org/wiki/Linear_regression}
#' @docType package
#' @name boxlinre
NULL





#' @title Multiple Linear Regression.
#' @description Multiple Linear Regression.
#' @param formula A formula implies the model (e.g. y ~ x).
#' @param data data frame, dataset attached to the algorithm.
#' @export
#' @return a list.

linreg <- function (formula, data) {
  stopifnot((class(formula)=="formula") && (class(data)=="data.frame"))

  X <- stats::model.matrix(formula, data)

  y_namn <- all.vars(formula, max.names=1)

  y <- data[[y_namn]]

  del_a <- t(X) %*% X
  del_b <- solve(del_a)

  reg_coef <- del_b %*% t(X) %*% y

  fitted_values <- X %*% reg_coef

  resi <- as.vector(y - fitted_values)

  n <- length(y)
  p <- length(colnames(X))
  deg_free <- n - p

  res_var <- as.vector((t(resi) %*% resi) / deg_free)

  var_reg_coef <- res_var * del_b

  t_each_coef <- reg_coef / sqrt(diag(var_reg_coef))

  p_values <- 2*stats::pt(-abs(t_each_coef), df = deg_free)


  ret <- list()
  class(ret) <- "linreg"
  ret$formula <- formula
  ret$data <- data
  ret$data_name <- deparse(substitute(data))
  ret$reg_coef <- reg_coef[,1]
  ret$fitted_values <- as.vector(fitted_values)
  ret$resi <- resi
  ret$deg_free <- deg_free
  ret$res_var <- res_var
  ret$var_reg_coef <- sqrt(diag(var_reg_coef))
  ret$t_each_coef <- t_each_coef
  ret$p_values <- p_values
  return(ret)
}

