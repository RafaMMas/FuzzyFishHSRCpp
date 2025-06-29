#' Weighted Mean Optimization for TSK-FRBS Consequents
#'
#' Optimizes the rule consequents of a Takagi–Sugeno–Kang (TSK) fuzzy rule-based system (FRBS)
#' using a weighted mean approach. Each consequent is calculated as the weighted mean of 
#' the observed outputs, weighted by the membership degree of each data point to the corresponding rule.
#'
#' @param Observed.output A numeric vector of observed output values (target variable).
#' @param data A data frame or matrix of input variables (predictors) used for evaluating membership.
#' @param FRBS A list representing the fuzzy rule-based system, where \code{FRBS[[1]]} contains membership functions
#' and \code{FRBS[[3]]} contains input variable limits.
#' @param case.weights Optional numeric vector of weights for each training instance. If \code{NULL}, equal weights are used.
#' @param Max.value Optional numeric value to assign to the rule with the highest consequent.
#' @param Min.value Optional numeric value to assign to the rule with the lowest consequent.
#' @param Rescale.to Optional numeric vector of length 2 indicating the range to rescale the consequents to (e.g., \code{c(0, 1)}).
#'
#' @return A numeric vector of optimized rule consequents, one for each fuzzy rule.
#'
#' @details
#' The function uses `evaluateFIS_cpp()` to compute membership values of each instance for all fuzzy rules.
#' Then, it computes the weighted mean of the observed outputs per rule, using membership degrees (and optional case weights)
#' as weights. Optionally, the consequents can be rescaled and/or clipped using `Max.value` and `Min.value`.
#'
#' @export
Weighted.mean.optimisation <- function(Observed.output,
                                       data,
                                       FRBS,
                                       case.weights = NULL,
                                       Max.value = 1,
                                       Min.value = NULL,
                                       Rescale.to = NULL){
  
if(is.null(case.weights))
  {
    case.weights <- rep(1, nrow(data))
  }

Membership <- evaluateFIS_cpp(data,
                              membershipFunctions = FRBS[[1]],
                              ruleConsequents = rep(1, prod(unlist(lapply(FRBS[[1]], function(x){length(x)})))),
                              inputLimits = FRBS[[3]])[,-1]

Consequents <- apply(Membership, 2 , function(x)
  {
    stats::weighted.mean(x = Observed.output, w = x*case.weights)
  })

if(!is.null(Max.value)){
  Consequents <- ((Consequents - min(Consequents))/(max(Consequents)-min(Consequents)))*(max(Rescale.to) - min(Rescale.to)) + min(Rescale.to)
}

if(!is.null(Max.value)){
  Consequents[which.max(Consequents)] <- Max.value
}

if(!is.null(Min.value)){
  Consequents[which.min(Consequents)] <- Min.value
}

return(Consequents)
  
}
