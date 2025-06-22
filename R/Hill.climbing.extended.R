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





