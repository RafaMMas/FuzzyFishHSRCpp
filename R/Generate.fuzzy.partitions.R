Generate.fuzzy.partitions <- function(values, var_names, mf_type, num_fuzzy_sets, inputLimits, extreme_values = c(-999, 999)) {
  # Validate MF type
  if (!(mf_type %in% c("trimf", "trapmf", "pimf"))) {
    stop("Only 'trimf', 'trapmf', or 'pimf' are allowed.")
  }
  
  fuzzy_partitions <- list()
  index <- 1  # Tracks position in `values` vector
  
  # Define extreme values (min and max)
  min_value <- extreme_values[1]
  max_value <- extreme_values[2]
  
  for (i in seq_along(var_names)) {
    # Get input limits for the current variable
    var_min <- inputLimits[[i]][1]
    var_max <- inputLimits[[i]][2]
    
    # Calculate the number of total parameters based on MF type and fuzzy set count
    if (mf_type == "trimf") {
      # The first fuzzy set uses 3 parameters and the subsequent ones use 2
      total_params <- 3*num_fuzzy_sets[i] - 4
    } else {  # 'trapmf' and 'pimf' each fuzzy set needs 4 parameters
      total_params <- 4*num_fuzzy_sets[i] - 4
    }

    # Prepend and append the extreme values to the breakpoints
    breakpoints <- c(min_value, var_min, values[index:(index + total_params - 1)], var_max, max_value)
    index <- index + total_params  # Update index to match the size
    
    # Generate membership functions
    mfs <- list()
    for (j in 1:num_fuzzy_sets[i]) {
      if (mf_type == "trimf") {
        if (j == 1) {
          # First fuzzy set: 3 parameters (lower, peak, upper)
          mfs[[j]] <- c(breakpoints[(j-1)*3 + 1], 
                        breakpoints[(j-1)*3 + 2], 
                        breakpoints[(j-1)*3 + 3])
        } else {
          # Subsequent fuzzy sets: 2 parameters (shared)
          mfs[[j]] <- c(breakpoints[(j-1)*2 + 2], 
                        breakpoints[(j-1)*2 + 3])
        }
      } else {  # For 'trapmf' and 'pimf', each fuzzy set requires 4 parameters
        mfs[[j]] <- c(breakpoints[(j-1)*4 + 1], 
                      breakpoints[(j-1)*4 + 2], 
                      breakpoints[(j-1)*4 + 3], 
                      breakpoints[(j-1)*4 + 4])
      }
    }
    
    fuzzy_partitions[[var_names[i]]] <- mfs
  }
  
  return(fuzzy_partitions)
}


