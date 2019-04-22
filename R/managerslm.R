#' @title Manager's Regression Analysis
#'
#' @description Implements the manager's regression by sorting the X_ik and Y_i values independently of each other.
#'
#' @description Provides better regressions most of the time.
#'
#' @param formula an object of class "formula" (or one that can be coerced to that class): a symbolic description of the model to be fitted.
#'
#' @param data a dataframe containing the variables in the model.
#'
#' @details Models for managers.lm are specified symbolically. A typical model has the form response ~ terms where response is the (numeric) response vector and terms is a series of terms which specifies a linear predictor for response. A terms specification of the form first + second indicates all the terms in first together with all the terms in second with duplicates removed. A specification of the form first:second indicates the set of terms obtained by taking the interactions of all terms in first with all terms in second. The specification first*second indicates the cross of first and second. This is the same as first + second + first:second.
#'
#' @details managers.lm differs from standard lm in that it indepdently sorts each independent and dependent variable specified in the model before running the regression. This provides better regressions most of the time.
#'
#' @return managers.lm returns an object of class "lm".
#'
#' @return The functions summary and anova are used to obtain and print a summary and analysis of variance table of the results. The generic accessor functions coefficients, effects, fitted.values and residuals extract various useful features of the value returned by lm.
#'
#' @return For more information regarding the contents of an object of class "lm," see the doucmentation for lm.
#'
#' @examples  managers.lm(Y ~ X, data = d)
#'
#' @export managers.lm

managers.lm <- function(formula, data){
  d <- as.data.frame(apply(data, 2, sort))
  m <- lm(formula, d)
  return(m)
}
