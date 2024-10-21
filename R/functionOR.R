#
#
#'@title Odds-Ratio and Confidence Interval for Logistic Regression predictors
#'@description input a function of binary variable Y given Xn predictors and
#' return output of odds ratio and confidence interval for each predictor variable
#' @param X description predictor variable
#' @return odds ratio and 95% confidence interval
#' @author Mohsyn Imran Malik
#' @examples
#' OR_95CI()


OR_95CI <- function(formula, data, siglevel, roundto){
  model <- glm(formula, data = data, family = binomial(link = "logit"))
  coef <- summary(model)$coefficients[,1]
  se <- summary(model)$coefficients[,2]
  q <- 1 - siglevel / 2
  OR <- numeric(length(coef)-1)
  ORlcl <- numeric(length(coef)-1)
  ORucl <- numeric(length(coef)-1)
  ORresult <- numeric(length(coef)-1)
  for (i in 1:(length(coef)-1)) {
    OR[i] <- exp(coef[i+1])
    ORlcl[i] <- exp(coef[i+1] - qnorm(q) * se[i+1])
    ORucl[i] <- exp(coef[i+1] + qnorm(q) * se[i+1])
    ORresult[i] <- paste0(format(round(OR[i], roundto), nsmall = roundto),
                       " (",
                       format(round(ORlcl[i], roundto), nsmall = roundto),
                       ", ",
                       format(round(ORucl[i], roundto), nsmall = roundto),
                       ")"
  )}
  return(ORresult)
}


