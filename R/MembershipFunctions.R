#' Gaussian membership function
#'
#' @param pattern the values of the input variable for whom the membership is calculated
#' @param parameters the parameters of the membership function
#'
#' @return membership of the evaluated values
#' @importFrom stats approx
#' @export
#'
#' @examples
#' plot(GAUSSMF(pattern = seq(0, 100, by = 1), parameters = c(35, 5)),
#' type = "l", xlab = "Value", ylab = "Membership", bty = "n", las = 1, col = "orangered")
#'
#' plot(GAUSSMF(pattern = seq(0, 100, by = 1), parameters = c(60, 25)),
#' type = "l", xlab = "Value", ylab = "Membership", bty = "n", las = 1, col = "orangered")
#'
GAUSSMF <- function(pattern, parameters) {
  exp(-1 / 2 * (pattern - parameters[1])^2 / parameters[2]^2)
}

#' Gaussian bell membership function
#'
#' @param pattern the values of the input variable for whom the membership is calculated
#' @param parameters the parameters of the membership function
#'
#' @return membership of the evaluated values
#' @importFrom stats approx
#' @export
#'
#' @examples
#' plot(GBELLMF(pattern = seq(0, 100, by = 1), parameters = c(10, 1, 50)),
#' type = "l", xlab = "Value", ylab = "Membership", bty = "n", las = 1, col = "orangered")
#'
#' plot(GBELLMF(pattern = seq(0, 100, by = 1), parameters = c(20, 5, 50)),
#' type = "l", xlab = "Value", ylab = "Membership", bty = "n", las = 1, col = "orangered")
#'
GBELLMF <- function(pattern, parameters) {
  1 / (1 + abs((pattern - parameters[3]) / parameters[1])^(2 * parameters[2]))
}

#' Triangular membership function
#'
#' @param pattern the values of the input variable for whom the membership is calculated
#' @param parameters the parameters of the membership function
#'
#' @return membership of the evaluated values
#' @importFrom stats approx
#' @export
#'
#' @examples
#' plot(TRIMF(pattern = seq(0, 100, by = 1), parameters = c(10, 60, 75)),
#' type = "l", xlab = "Value", ylab = "Membership", bty = "n", las = 1, col = "orangered")
#'
#' plot(TRIMF(pattern = seq(0, 100, by = 1), parameters = c(10, 40, 70)),
#' type = "l", xlab = "Value", ylab = "Membership", bty = "n", las = 1, col = "orangered")
#'
TRIMF <- function(pattern, parameters) {
  approx(x = parameters, y = c(0, 1, 0), xout = pattern, method = "linear", rule = c(2, 2), ties = max)$y
}

#' Trapezoidal membership function
#'
#' @param pattern the values of the input variable for whom the membership is calculated
#' @param parameters the parameters of the membership function
#'
#' @return membership of the evaluated values
#' @importFrom stats approx
#' @export
#'
#' @examples
#' plot(TRAPMF(pattern = seq(0, 100, by = 1), parameters = c(10, 60, 75, 90)),
#' type = "l", xlab = "Value", ylab = "Membership", bty = "n", las = 1, col = "orangered")
#'
#' plot(TRAPMF(pattern = seq(0, 100, by = 1), parameters = c(10, 40, 50, 80)),
#' type = "l", xlab = "Value", ylab = "Membership", bty = "n", las = 1, col = "orangered")
#'
TRAPMF <- function(pattern, parameters) {
  approx(x = parameters, y = c(0, 1, 1, 0), xout = pattern, method = "linear", rule = c(2, 2), ties = max)$y
}

#' Sigmoidal membership functions
#'
#' @param pattern the values of the input variable for whom the membership is calculated
#' @param parameters the parameters of the membership function
#'
#' @return membership of the evaluated values
#' @importFrom stats approx
#' @export
#'
#' @examples
#' plot(SIGMF(pattern = seq(0, 100, by = 1), parameters = c(0.5,50)),
#' type = "l", xlab = "Value", ylab = "Membership", bty = "n", las = 1, col = "orangered")
#'
#' plot(SIGMF(pattern = seq(0, 100, by = 1), parameters = c(-0.2, 35)),
#' type = "l", xlab = "Value", ylab = "Membership", bty = "n", las = 1, col = "orangered")
#'
SIGMF <- function(pattern, parameters) {
  1 / (1 + exp(-parameters[1] * (pattern - parameters[2])))
}

#' Double sigmoidal membership function
#'
#' @param pattern the values of the input variable for whom the membership is calculated
#' @param parameters the parameters of the membership function
#'
#' @return membership of the evaluated values
#' @importFrom stats approx
#' @export
#'
#' @examples
#' plot(DSIGMF(pattern = seq(0, 100, by = 1), parameters = c(2, 20, 0.5, 60)),
#' type = "l", xlab = "Value", ylab = "Membership", bty = "n", las = 1, col = "orangered")
#'
#' plot(DSIGMF(pattern = seq(0, 100, by = 1), parameters = c(0.5, 20, 0.25, 50)),
#' type = "l", xlab = "Value", ylab = "Membership", bty = "n", las = 1, col = "orangered")
#'
DSIGMF <- function(pattern, parameters) {
  abs(1 / (1 + exp(-parameters[1] * (pattern - parameters[2]))) - 1 / (1 + exp(-parameters[3] * (pattern - parameters[4]))))
}

#' Product of two sigmoidal membership functions
#'
#' @param pattern the values of the input variable for whom the membership is calculated
#' @param parameters the parameters of the membership function
#'
#' @return membership of the evaluated values
#' @importFrom stats approx
#' @export
#'
#' @examples
#' plot(PSIGMF(pattern = seq(0, 100, by = 1), parameters = c(2, 15, -0.5, 60)),
#' type = "l", xlab = "Value", ylab = "Membership", bty = "n", las = 1, col = "orangered")
#'
#' plot(PSIGMF(pattern = seq(0, 100, by = 1), parameters = c(1, 50, -0.5, 75)),
#' type = "l", xlab = "Value", ylab = "Membership", bty = "n", las = 1, col = "orangered")
#'
PSIGMF <- function(pattern, parameters) {
  1 / (1 + exp(-parameters[1] * (pattern - parameters[2]))) * 1 / (1 + exp(-parameters[3] * (pattern - parameters[4])))
}

#' Pi membership functions
#'
#' @param pattern the values of the input variable for whom the membership is calculated
#' @param parameters the parameters of the membership function
#'
#' @return membership of the evaluated values
#' @importFrom stats approx
#' @export
#'
#' @examples
#' plot(PIMF(pattern = seq(0, 100, by = 1), parameters = c(10, 60, 75, 90)),
#' type = "l", xlab = "Value", ylab = "Membership", bty = "n", las = 1, col = "orangered")
#'
#' plot(PIMF(pattern = seq(0, 100, by = 1), parameters = c(10, 40, 50, 80)),
#' type = "l", xlab = "Value", ylab = "Membership", bty = "n", las = 1, col = "orangered")
#'
PIMF <- function(pattern, parameters) {
  Membership <- c()
  for (z in 1:length(pattern))
  {
    # plateau
    if (pattern[z] >= parameters[2] & pattern[z] <= parameters[3]) {
      y <- 1
    }

    # Steep descend
    if (pattern[z] >= mean(parameters[1:2]) & pattern[z] < parameters[2]) {
      y <- 1 - 2 * ((pattern[z] - parameters[2]) / (diff(parameters[1:2])))^2
    }

    if (pattern[z] > parameters[3] & pattern[z] <= mean(parameters[3:4])) {
      y <- 1 - 2 * ((pattern[z] - parameters[3]) / (diff(parameters[3:4])))^2
    }

    # Smooth descend
    if (pattern[z] >= parameters[1] & pattern[z] < mean(parameters[1:2])) {
      y <- 2 * ((pattern[z] - parameters[1]) / (diff(parameters[1:2])))^2
    }

    if (pattern[z] > mean(parameters[3:4]) & pattern[z] <= parameters[4]) {
      y <- 2 * ((pattern[z] - parameters[4]) / (parameters[4] - parameters[3]))^2
    }

    # External value
    if (pattern[z] < parameters[1] | pattern[z] > parameters[4]) {
      y <- 0
    }

    Membership <- c(Membership, y)
  }
  return(Membership)
}

#' Z-shaped membership function
#'
#' @param pattern the values of the input variable for whom the membership is calculated
#' @param parameters the parameters of the membership function
#'
#' @return membership of the evaluated values
#' @importFrom stats approx
#' @export
#'
#' @examples
#' plot(ZMF(pattern = seq(0, 100, by = 1), parameters = c(10, 60)),
#' type = "l", xlab = "Value", ylab = "Membership", bty = "n", las = 1, col = "orangered")
#'
#' plot(ZMF(pattern = seq(0, 100, by = 1), parameters = c(0.5, 80)),
#' type = "l", xlab = "Value", ylab = "Membership", bty = "n", las = 1, col = "orangered")
#'
ZMF <- function(pattern, parameters) {
  Membership <- c()
  for (z in 1:length(pattern))
  {
    if (pattern[z] <= parameters[1]) {
      y <- 1
    }
    # 5
    if (pattern[z] > parameters[1] & pattern[z] <= mean(parameters[1:2])) {
      y <- 1 - 2 * ((pattern[z] - parameters[1]) / (diff(parameters[1:2])))^2
    }
    # 6
    if (pattern[z] > mean(parameters[1:2]) & pattern[z] <= parameters[2]) {
      y <- 2 * ((pattern[z] - parameters[2]) / (parameters[2] - parameters[1]))^2
    }
    # 7
    if (pattern[z] > parameters[2]) {
      y <- 0
    }
    Membership <- c(Membership, y)
  }
  Membership
}

#' S-shaped membership function
#'
#' @param pattern the values of the input variable for whom the membership is calculated
#' @param parameters the parameters of the membership function
#'
#' @return membership of the evaluated values
#' @importFrom stats approx
#' @export
#'
#' @examples
#' plot(SMF(pattern = seq(0, 100, by = 1), parameters = c(10, 60)),
#' type = "l", xlab = "Value", ylab = "Membership", bty = "n", las = 1, col = "orangered")
#'
#' plot(SMF(pattern = seq(0, 100, by = 1), parameters = c(0.5, 90)),
#' type = "l", xlab = "Value", ylab = "Membership", bty = "n", las = 1, col = "orangered")
#'
SMF <- function(pattern, parameters) {
  Membership <- c()
  for (z in 1:length(pattern))
  {
    if (pattern[z] < parameters[1]) {
      y <- 0
    }
    # 2
    if (pattern[z] >= parameters[1] & pattern[z] < mean(parameters[1:2])) {
      y <- 2 * ((pattern[z] - parameters[1]) / (diff(parameters[1:2])))^2
    }
    # 3
    if (pattern[z] >= mean(parameters[1:2]) & pattern[z] < parameters[2]) {
      y <- 1 - 2 * ((pattern[z] - parameters[2]) / (diff(parameters[1:2])))^2
    }
    # 4
    if (pattern[z] >= parameters[2]) {
      y <- 1
    }
    Membership <- c(Membership, y)
  }
  Membership
}
