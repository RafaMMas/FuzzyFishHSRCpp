#' Compute Membership Function (MF) Values (Internal Helper)
#'
#' Internal helper function to compute the membership value(s) for a given input
#' using a specified membership function (MF) type and parameters. Not intended
#' for use outside of other functions within the package.
#'
#' Supported MF types:
#' \itemize{
#'   \item \code{"trimf"}: Triangular MF. \code{params = c(a, b, c)}
#'   \item \code{"trapmf"}: Trapezoidal MF. \code{params = c(a, b, c, d)}
#'   \item \code{"gaussmf"}: Gaussian MF. \code{params = c(mean, sigma)}
#'   \item \code{"sigmf"}: Sigmoidal MF. \code{params = c(a, c)} where \code{a} is slope, \code{c} is center
#'   \item \code{"gbellmf"}: Generalized bell MF. \code{params = c(a, b, c)}
#'   \item \code{"zmf"}: Z-shaped MF. \code{params = c(a, b)}
#'   \item \code{"smf"}: S-shaped MF. \code{params = c(a, b)}
#'   \item \code{"pimf"}: PI-shaped MF. \code{params = c(a, b, c, d)}
#' }
#'
#' @param x Numeric. Input value or vector of values at which to compute the membership.
#' @param type Character. The type of membership function to use (e.g., "trimf", "trapmf", etc.).
#' @param params Numeric vector. Parameters for the specified membership function. See details above.
#'
#' @return A numeric vector of membership values corresponding to input \code{x}.
#'
#' @examples
#' computeMFValue(0.5, "trimf", c(0, 0.5, 1))
#' computeMFValue(seq(0, 1, 0.1), "gaussmf", c(0.5, 0.1))
#'
#' @keywords internal
computeMFValue <- function(x, type, params) {
  if (type == "trimf") {
    # Triangular MF: params = c(a, b, c)
    a <- params[1]; b <- params[2]; c <- params[3]
    y <- ifelse(x <= a | x >= c, 0,
                ifelse(x < b, (x - a) / (b - a), (c - x) / (c - b)))
    return(y)
  } else if (type == "trapmf") {
    # Trapezoidal MF: params = c(a, b, c, d)
    a <- params[1]; b <- params[2]; c <- params[3]; d <- params[4]
    y <- ifelse(x <= a | x >= d, 0,
                ifelse(x < b, (x - a) / (b - a),
                       ifelse(x > c, (d - x) / (d - c), 1)))
    return(y)
  } else if (type == "gaussmf") {
    # Gaussian MF: params = c(mean, sigma)
    m <- params[1]; sigma <- params[2]
    return(exp(-0.5 * ((x - m) / sigma)^2))
  } else if (type == "sigmf") {
    # Sigmoidal MF: params = c(a, c) where a is slope, c is center
    a <- params[1]; c_param <- params[2]
    return(1 / (1 + exp(-a * (x - c_param))))
  } else if (type == "gbellmf") {
    # Generalized bell MF: params = c(a, b, c)
    a <- params[1]; b <- params[2]; c_param <- params[3]
    return(1 / (1 + abs((x - c_param) / a)^(2 * b)))
  } else if (type == "zmf") {
    # Z-shaped MF: params = c(a, b)
    a <- params[1]; b <- params[2]
    y <- ifelse(x <= a, 1,
                ifelse(x >= b, 0, 1 - ((x - a) / (b - a))^2))
    return(y)
  } else if (type == "smf") {
    # S-shaped MF: params = c(a, b)
    a <- params[1]; b <- params[2]
    y <- ifelse(x <= a, 0,
                ifelse(x >= b, 1, ((x - a) / (b - a))^2))
    return(y)
  } else if (type == "pimf") {
    # PI-shaped MF: params = c(a, b, c, d)
    # Definition:
    #   0,                       x <= a
    #   2*((x-a)/(b-a))^2,       a < x <= (a+b)/2
    #   1-2*((x-b)/(b-a))^2,     (a+b)/2 < x <= b
    #   1,                       b < x <= c
    #   1-2*((x-c)/(d-c))^2,     c < x <= (c+d)/2
    #   2*((x-d)/(d-c))^2,       (c+d)/2 < x < d
    #   0,                       x >= d
    a <- params[1]; b <- params[2]; c <- params[3]; d <- params[4]
    return(
      ifelse(x <= a, 0,
             ifelse(x <= (a + b) / 2,
                    2 * ((x - a) / (b - a))^2,
                    ifelse(x <= b,
                           1 - 2 * ((x - b) / (b - a))^2,
                           ifelse(x <= c,
                                  1,
                                  ifelse(x <= (c + d) / 2,
                                         1 - 2 * ((x - c) / (d - c))^2,
                                         ifelse(x < d,
                                                2 * ((x - d) / (d - c))^2,
                                                0))))))
    )
  } else {
    stop("Unknown membership function type.")
  }
}

#' Plot Membership Functions for a Single Variable
#'
#' Plot all membership functions defined for a single input variable.
#'
#' This function helps visualize fuzzy membership functions of a variable using
#' base R plotting. Each curve is labeled in a legend according to its type and parameters.
#'
#' @param mfList A list of membership functions. Each element should be a list
#'   with components \code{type} (character) and \code{params} (numeric vector).
#' @param variableName Character string. Optional name for the variable being plotted.
#' @param xlim Numeric vector of length 2. Range of the x-axis (default is \code{c(0, 1)}).
#' @param n Integer. Number of evaluation points along the x-axis (default is 100).
#' @param colors Optional character vector of colors to use for the functions.
#' @param lwd Numeric. Line width for the membership function curves (default is 2).
#' @param main Character. Plot title. If \code{NULL}, a title will be generated automatically.
#' @param xlab Character. Label for the x-axis.
#' @param ylab Character. Label for the y-axis.
#' @param Legend Logical. If \code{TRUE}, a legend is added to the plot; if \code{FALSE}, it is omitted.
#' @param ... Additional graphical parameters passed to \code{\link[graphics]{plot}}.
#'
#' @return A plot of membership functions for one variable.
#'
#' @examples
#' mfList <- list(
#'   list(type = "gaussmf", params = c(0.25, 0.1)),
#'   list(type = "gaussmf", params = c(0.75, 0.1))
#' )
#' 
#' inputLimits <- list(X1 = c(-0.25, 1.25))
#' 
#' plotVariableMFs(mfList, variableName = "Example Variable", xlim = inputLimits)
#'
#' @export
plotVariableMFs <- function(mfList, variableName = "", xlim = NULL, n = 100,
                            colors = NULL, lwd = 2, main = NULL,
                            xlab = "x", ylab = "Membership", Legend = TRUE, ...) {
  x <- seq(xlim[[1]][1], xlim[[1]][2], length.out = n)
  if (is.null(colors)) {
    colors <- grDevices::rainbow(length(mfList))
  }
  if (is.null(main)) {
    main <- paste("Membership Functions for", variableName)
  }
  # Create an empty plot
  plot(x, rep(0, length(x)), type = "n", ylim = c(0, 1),
       xlab = xlab, ylab = ylab, main = main, ...)
  
  # Plot each membership function
  legendText <- character(length(mfList))
  for (i in seq_along(mfList)) {
    mf <- mfList[[i]]
    y <- computeMFValue(x, mf$type, mf$params)
    graphics::lines(x, y, col = colors[i], lwd = lwd)
    legendText[i] <- paste0(mf$type, " (", paste(round(mf$params,1), collapse = ","), ")")
  }
  if(Legend == T)
  graphics::legend("topright", legend = legendText, col = colors, lwd = lwd, cex = 0.8)
}

#' Plot Membership Functions for All Variables
#'
#' Plot membership functions for multiple variables, each in a separate panel.
#'
#' This function visualizes fuzzy membership functions grouped by variable.
#' It arranges the plots in a grid layout using base R graphics.
#'
#' @param membershipFunctions A list of lists. Each element represents one variable,
#'   and should be a list of membership functions as used in \code{\link{plotVariableMFs}}.
#' @param varNames Optional character vector with names for each variable.
#' @param xlim Numeric vector of length 2. Common x-axis range for all plots.
#' @param n Integer. Number of evaluation points (default is 100).
#' @param colors Optional character vector of colors.
#' @param lwd Numeric. Line width for plotted curves.
#' @param main Character. Prefix for titles in each plot.
#' @param xlab Character. X-axis label.
#' @param ylab Character. Y-axis label.
#' @param ... Additional graphical parameters passed to \code{\link{plotVariableMFs}}.
#'
#' @return A grid of plots, one for each variable.
#'
#' @examples
#' mf1 <- list(
#'   list(type = "gaussmf", params = c(0.25, 0.1)),
#'   list(type = "gaussmf", params = c(0.75, 0.1))
#' )
#' mf2 <- list(
#'   list(type = "trapmf", params = c(0, 0.3, 0.7, 1)),
#'   list(type = "trapmf", params = c(0.7, 1, 1.5, 2))
#' )
#' 
#' inputLimits <- list(X1 = c(-0.25, 1.25), X2 = c(0, 2))
#'
#' plotAllMFs(list(mf1, mf2), varNames = c("X1", "X2"), xlim = inputLimits)
#'
#' @export
plotAllMFs <- function(membershipFunctions, varNames = NULL, xlim = NULL, n = 100,
                       colors = NULL, lwd = 2, main = NULL,
                       xlab = "x", ylab = "Membership", ...) {
  nVars <- length(membershipFunctions)
  if (is.null(varNames)) {
    varNames <- paste0("Variable ", seq_len(nVars))
  }
  
  # Set up plotting area: one panel per variable
  op <- graphics::par(no.readonly = TRUE)
  graphics::par(mfrow = c(ceiling(sqrt(nVars)), ceiling(nVars / ceiling(sqrt(nVars)))))
  
  # Plot membership functions for each variable
  for (i in seq_along(membershipFunctions)) {
    plotVariableMFs(membershipFunctions[[i]],
                    variableName = varNames[i],
                    xlim = xlim[i], n = n, colors = colors,
                    lwd = lwd, main = paste(main, ifelse(is.null(main), "", "-"), varNames[i]),
                    xlab = xlab, ylab = ylab, ...)
  }
  
  # Reset plotting parameters
  graphics::par(op)
}
