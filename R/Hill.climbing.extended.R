#' Hill-Climbing Optimization with Restarts
#'
#' Performs an extended version of the hill-climbing algorithm with support
#' for multiple random restarts. This optimization method iteratively explores
#' neighboring sequences in search of a maximum or minimum of a user-defined
#' objective function.
#'
#' @param func A function that takes a numeric vector as input and returns a scalar value.
#'   This is the objective function to be optimized.
#' @param values A numeric vector specifying the allowed values in the solution sequence.
#'   Defaults to \code{c(0, 0.2, 0.4, 0.6, 0.8, 1)}.
#' @param size An integer specifying the length of the solution sequence.
#' @param mode A character string indicating whether to \code{"max"}imize or \code{"min"}imize
#'   the objective function. Default is \code{"max"}.
#' @param restarts An integer specifying how many independent random restarts to perform.
#'   Higher values increase the chance of escaping local optima. Default is 1.
#' @param seed Optional integer seed for reproducibility of random number generation.
#'
#' @return A list with two elements:
#'   \describe{
#'     \item{\code{optimal_sequence}}{The best sequence of values found across all restarts.}
#'     \item{\code{optimal_value}}{The corresponding value of the objective function.}
#'   }
#'
#' @examples
#' # Define a simple objective function
#' f <- function(x) -sum((x - 0.5)^2)
#'
#' # Run hill-climbing to maximize f
#' result <- Hill.climbing.extended(f, size = 6, restarts = 5, seed = 42)
#' print(result)
#'
#' @export
Hill.climbing.extended <- function(func, values = c(0, 0.2, 0.4, 0.6, 0.8, 1), size = 8, mode = "max", restarts = 1, seed = NULL) {
  if (!mode %in% c("max", "min")) stop("mode must be 'max' or 'min'")
  if (!is.null(seed)) set.seed(seed)  # Set seed for reproducibility
  
  best_overall_sequence <- NULL
  best_overall_value <- if (mode == "max") -Inf else Inf
  
  for (r in 1:restarts) {
    # Generate a new random starting sequence for each restart
    current <- sample(values, size, replace = TRUE)
    current_value <- func(current)
    improvement <- TRUE  
    
    while (improvement) {
      improvement <- FALSE  
      best_neighbor <- current
      best_value <- current_value
      
      # Shuffle index order to test elements randomly
      index_order <- sample(1:length(current))  
      for (j in index_order) {
        value_order <- sample(values)  
        
        for (val in value_order) {
          if (val != current[j]) {
            new_neighbor <- current
            new_neighbor[j] <- val
            new_value <- func(new_neighbor)
            
            # Compare based on mode (maximize or minimize)
            if ((mode == "max" && new_value > best_value) || 
                (mode == "min" && new_value < best_value)) {
              best_neighbor <- new_neighbor
              best_value <- new_value
              improvement <- TRUE  
            }
          }
        }
      }
      
      # Move to the best neighbor
      if (improvement) {
        current <- best_neighbor
        current_value <- best_value
      }
    }
    
    # Keep track of the best overall result across restarts
    if ((mode == "max" && current_value > best_overall_value) ||
        (mode == "min" && current_value < best_overall_value)) {
      best_overall_sequence <- current
      best_overall_value <- current_value
    }
  }
  
  return(list(optimal_sequence = best_overall_sequence, optimal_value = best_overall_value))
}





