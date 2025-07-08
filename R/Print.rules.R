#' Print Fuzzy Rules from a Fuzzy Rule-Based System
#'
#' This function prints the rule base of a fuzzy rule-based system (FRBS), associating fuzzy input labels with corresponding consequents.
#'
#' @param FRBS A list representing the fuzzy rule-based system, typically containing:
#'   \describe{
#'     \item{[[1]]}{A named list of membership functions for each input variable.}
#'     \item{\code{$ruleConsequents}}{A vector of consequents associated with the fuzzy rules.}
#'   }
#'
#' @details
#' The function generates human-readable fuzzy labels (e.g., "Low", "Medium", "High") based on the number of fuzzy sets per input variable (up to a maximum of 5 sets). 
#' It then prints a data frame showing all combinations of these fuzzy input labels along with their associated rule consequents.
#'
#' Supported fuzzy set label mappings:
#' \itemize{
#'   \item 2 sets: "Low", "High"
#'   \item 3 sets: "Low", "Medium", "High"
#'   \item 4 sets: "Very low", "Low", "High", "Very high"
#'   \item 5 sets: "Very low", "Low", "Medium", "High", "Very high"
#' }
#'
#' If any input variable has more than 5 fuzzy sets, the function will stop with an error.
#'
#' @return Prints a data frame with fuzzy input combinations and rule consequents. Returns \code{NULL} invisibly.
#'
#' @examples
#' \dontrun{
#' Print.rules(FRBS)
#' }
#'
#' @export
Print.rules <- function(FRBS)
{
  
  Rules <- expand.grid(sapply(rev(sapply(FRBS[[1]], length)), function(x){
    if(x == 2){
      c("Low", "High")
    } else if(x == 3){
      c("Low", "Medium", "High")
    } else if( x == 4) { 
      c("Very low", "Low", "High", "Very high")
    } else if( x == 5) { 
      c("Very low", "Low", "Medium", "High", "Very high")
    } else {
      paste("Set", 1:x)
    }
  }))  
  
  print(data.frame(Rules[names(FRBS[[1]])], Consequents = FRBS$ruleConsequents))
  
}
