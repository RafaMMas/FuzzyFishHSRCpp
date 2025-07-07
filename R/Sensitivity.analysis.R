#' Perform Sensitivity Analysis on Fuzzy Rule-Based System
#'
#' This function performs a sensitivity analysis on a fuzzy rule-based system (FRBS) by varying selected input variables over a specified range and computing the resulting model outputs.
#'
#' @param FRBS A list representing the fuzzy rule-based system. It should contain three elements: 
#'   \describe{
#'     \item{[[1]]}{Membership functions}
#'     \item{[[2]]}{Rule consequents}
#'     \item{[[3]]}{Input limits}
#'   }
#' @param data A data frame of input data to the model.
#' @param Selected.variable A character vector of variable names to include in the sensitivity analysis. If \code{NULL}, all variables in \code{data} are used.
#' @param n.pt An integer specifying the number of points over which to evaluate the sensitivity for each variable. Defaults to 20.
#'
#' @return A named list of data frames, each corresponding to a variable in \code{Selected.variable}. Each data frame includes:
#'   \describe{
#'     \item{x.pt}{Values over which the variable was varied}
#'     \item{mean.Suitability}{Mean predicted output for each value}
#'     \item{0 to 1 (quantiles)}{Quantiles (0% to 100%) of predicted outputs}
#'   }
#'
#' @details For each selected variable, the function varies its value across a range (min to max) while holding other variables constant, and evaluates the FRBS model at each point. It then calculates the mean and quantiles of the predicted outputs.
#'
#' @examples
#' \dontrun{
#' result <- Sensitivity.analysis(FRBS, input_data)
#' plot(result[["Variable1"]][,1], result[["Variable1"]][,"mean.Suitability"])
#' }
#'
#' @export
Sensitivity.analysis <- function(FRBS, data, Selected.variable = NULL, n.pt = 20){
  
  if(is.null(Selected.variable))
    Selected.variable <- colnames(data)
  
  Sensitivity.analysis <- list()
  
  for(ii in Selected.variable)
  {
    pred.data <- data
    n.pt <-  ifelse(length(unique(data[,ii])) < n.pt, length(unique(data[,ii])), n.pt)
    xv <- pred.data[, ii]
    n <- nrow(pred.data)
    x.pt <- seq(min(xv), max(xv), length = n.pt)
    y.pt <- c()
    
    for(i in seq(along = x.pt))
    {
      x.data <- pred.data
      x.data[, ii] <- x.pt[i]
      Predict <- evaluateFIS_cpp(inputData = x.data, membershipFunctions = FRBS[[1]], ruleConsequents = FRBS[[2]], inputLimits = FRBS[[3]])[,1]
      y.pt <- rbind(y.pt, c(mean(Predict), quantile(Predict, probs=seq(0, 1, by=0.05))))
    }
    
    Sensitivity.analysis[[length(Sensitivity.analysis)+1]] <- setNames(data.frame(x.pt, y.pt), c(ii, "mean.Suitability", seq(0, 1, by=0.05)))
  }
  
  names(Sensitivity.analysis) <- Selected.variable
  
  return(Sensitivity.analysis)
  
}
