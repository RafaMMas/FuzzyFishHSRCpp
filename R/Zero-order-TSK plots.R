# Helper function to compute membership values
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

# Function to plot all membership functions for one variable
plotVariableMFs <- function(mfList, variableName = "", xlim = c(0, 1), n = 100,
                            colors = NULL, lwd = 2, main = NULL,
                            xlab = "x", ylab = "Membership Degree", ...) {
  x <- seq(xlim[1], xlim[2], length.out = n)
  if (is.null(colors)) {
    colors <- rainbow(length(mfList))
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
    lines(x, y, col = colors[i], lwd = lwd)
    legendText[i] <- paste0(mf$type, " (", paste(mf$params, collapse = ","), ")")
  }
  
  legend("topright", legend = legendText, col = colors, lwd = lwd, cex = 0.8)
}

# Function to plot membership functions for all variables
plotAllMFs <- function(membershipFunctions, varNames = NULL, xlim = c(0, 1), n = 100,
                       colors = NULL, lwd = 2, main = "Membership Functions",
                       xlab = "x", ylab = "Membership Degree", ...) {
  nVars <- length(membershipFunctions)
  if (is.null(varNames)) {
    varNames <- paste0("Variable ", seq_len(nVars))
  }
  
  # Set up plotting area: one panel per variable
  op <- par(no.readonly = TRUE)
  par(mfrow = c(ceiling(sqrt(nVars)), ceiling(nVars / ceiling(sqrt(nVars)))))
  
  # Plot membership functions for each variable
  for (i in seq_along(membershipFunctions)) {
    plotVariableMFs(membershipFunctions[[i]],
                    variableName = varNames[i],
                    xlim = xlim, n = n, colors = colors,
                    lwd = lwd, main = paste(main, "-", varNames[i]),
                    xlab = xlab, ylab = ylab, ...)
  }
  
  # Reset plotting parameters
  par(op)
}
