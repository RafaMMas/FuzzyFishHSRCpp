#' Generate Fuzzy Partitions for Input Variables
#'
#' Constructs fuzzy membership functions (MFs) for each input variable using
#' predefined breakpoints and a specified MF type.
#'
#' The function supports `trimf`, `trapmf`, and `pimf` types, and generates
#' fuzzy sets based on user-defined values and input limits. This is typically
#' used for building interpretable fuzzy systems.
#'
#' @param values A numeric vector of breakpoint values used to construct the MFs.
#'   Must contain the correct number of parameters based on the MF type and number
#'   of fuzzy sets for each variable.
#' @param var_names A character vector with names of the input variables.
#' @param mf_type A character string specifying the type of membership function.
#'   Must be one of \code{"trimf"}, \code{"trapmf"}, or \code{"pimf"}.
#' @param num_fuzzy_sets An integer vector giving the number of fuzzy sets per variable.
#'   Must be the same length as \code{var_names}.
#' @param inputLimits A list of numeric vectors (length 2) specifying the minimum and maximum
#'   values for each input variable.
#' @param extreme_values A numeric vector of length 2 specifying the artificial minimum and
#'   maximum bounds used to pad the breakpoint vector. Default is \code{c(-999, 999)}.
#'
#' @return A named list of fuzzy partitions. Each element is a list of fuzzy sets
#'   (vectors of parameters), with variable names as list names.
#'
#' @examples
#' # Example with two variables, both with 3 fuzzy sets and trimf type
#' values <- seq(0.2, 0.8, length.out = 8)  # Adjusted to match required param count
#' var_names <- c("Temperature", "Humidity")
#' mf_type <- "trimf"
#' num_fuzzy_sets <- c(3, 3)
#' inputLimits <- list(c(0, 1), c(0, 1))
#'
#' fuzzy_partitions <- Generate.fuzzy.partitions(
#'   values = values,
#'   var_names = var_names,
#'   mf_type = mf_type,
#'   num_fuzzy_sets = num_fuzzy_sets,
#'   inputLimits = inputLimits
#' )
#'
#' str(fuzzy_partitions)
#'
#' @export
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


