#include <Rcpp.h>
#include <cmath>
#include <vector>
using namespace Rcpp;

// --- Membership Function Definitions ---

// Triangular membership function: params = (a, b, c)
double trimf(double x, double a, double b, double c) {
  if (x <= a || x >= c) return 0.0;
  if (x < b) return (x - a) / (b - a);
  return (c - x) / (c - b);
}

// Trapezoidal membership function: params = (a, b, c, d)
double trapmf(double x, double a, double b, double c, double d) {
  if (x <= a || x >= d) return 0.0;
  if (x < b) return (x - a) / (b - a);
  if (x > c) return (d - x) / (d - c);
  return 1.0;
}

// Gaussian membership function: params = (mean, sigma)
double gaussmf(double x, double mean, double sigma) {
  return std::exp(-0.5 * std::pow((x - mean) / sigma, 2));
}

// Generalized bell membership function: params = (a, b, c)
double gbellmf(double x, double a, double b, double c) {
  return 1.0 / (1.0 + std::pow(std::abs((x - c) / a), 2 * b));
}

// Sigmoidal membership function: params = (a, c)
double sigmf(double x, double a, double c) {
  return 1.0 / (1.0 + std::exp(-a * (x - c)));
}

// Z-shaped membership function: params = (a, b)
double zmf(double x, double a, double b) {
  if (x <= a) return 1.0;
  if (x >= b) return 0.0;
  return 1.0 - std::pow((x - a) / (b - a), 2);
}

// S-shaped membership function: params = (a, b)
double smf(double x, double a, double b) {
  if (x <= a) return 0.0;
  if (x >= b) return 1.0;
  return std::pow((x - a) / (b - a), 2);
}

// PI-shaped membership function: params = (a, b, c, d)
// Definition: 
//   0,                       x <= a
//   2*((x-a)/(b-a))^2,       a < x <= (a+b)/2
//   1-2*((x-b)/(b-a))^2,     (a+b)/2 < x <= b
//   1,                       b < x <= c
//   1-2*((x-c)/(d-c))^2,     c < x <= (c+d)/2
//   2*((x-d)/(d-c))^2,       (c+d)/2 < x < d
//   0,                       x >= d
double pimf(double x, double a, double b, double c, double d) {
  if (x <= a) return 0.0;
  if (x <= (a + b) / 2.0)
    return 2 * std::pow((x - a) / (b - a), 2);
  if (x <= b)
    return 1 - 2 * std::pow((x - b) / (b - a), 2);
  if (x <= c)
    return 1.0;
  if (x <= (c + d) / 2.0)
    return 1 - 2 * std::pow((x - c) / (d - c), 2);
  if (x < d)
    return 2 * std::pow((x - d) / (d - c), 2);
  return 0.0;
}

// --- Function to Compute Membership Degree ---
// Now includes support for pimf.
double computeMembership(double x, std::string type, NumericVector params) {
  if (type == "trimf")
    return trimf(x, params[0], params[1], params[2]);
  if (type == "trapmf")
    return trapmf(x, params[0], params[1], params[2], params[3]);
  if (type == "gaussmf")
    return gaussmf(x, params[0], params[1]);
  if (type == "gbellmf")
    return gbellmf(x, params[0], params[1], params[2]);
  if (type == "sigmf")
    return sigmf(x, params[0], params[1]);
  if (type == "zmf")
    return zmf(x, params[0], params[1]);
  if (type == "smf")
    return smf(x, params[0], params[1]);
  if (type == "pimf")
    return pimf(x, params[0], params[1], params[2], params[3]);
  return 0.0;
}

// --- Main FIS Evaluation Function ---
// Now the input clamping is done once per variable for each observation,
// before calculating the membership degrees.
// The function accepts an optional parameter 'inputLimits'.
// 'inputLimits' is a list of length equal to the number of variables;
// each element is a numeric vector of length 2: c(lower, upper).
//
// The function constructs the full rule base (as the cross-product of the MFs for each variable),
// computes rule activations (using product aggregation), and defuzzifies via weighted average.
// [[Rcpp::export]]
NumericMatrix evaluateFIS_cpp(DataFrame inputData,
                              List membershipFunctions,
                              NumericVector ruleConsequents,
                              Nullable<List> inputLimits = R_NilValue) {
  int n = inputData.nrows();             // number of observations
  int nVars = membershipFunctions.size();  // number of input variables
  
  // Prepare clamping limits for each variable if provided.
  std::vector<bool> hasLimits(nVars, false);
  std::vector<double> lowerLimits(nVars), upperLimits(nVars);
  if (inputLimits.isNotNull()) {
    List limList(inputLimits);
    if (limList.size() != nVars)
      stop("Length of inputLimits must equal the number of input variables.");
    for (int j = 0; j < nVars; j++) {
      NumericVector lim = limList[j];
      if (lim.size() != 2)
        stop("Each element of inputLimits must be a numeric vector of length 2.");
      lowerLimits[j] = lim[0];
      upperLimits[j] = lim[1];
      hasLimits[j] = true;
    }
  }
  
  // Determine the number of MFs per variable and total number of rules.
  std::vector<int> mVec(nVars);
  int totalRules = 1;
  for (int j = 0; j < nVars; j++) {
    List varMFs = membershipFunctions[j];
    mVec[j] = varMFs.size();
    totalRules *= mVec[j];
  }
  
  // Check that ruleConsequents has the proper length.
  if (ruleConsequents.size() != totalRules)
    stop("Length of ruleConsequents must equal the total number of rules (product of the number of MFs per variable).");
  
  // Pre-extract each column from inputData.
  std::vector<NumericVector> columns(nVars);
  for (int j = 0; j < nVars; j++) {
    columns[j] = as<NumericVector>(inputData[j]);
  }
  
  // Prepare output matrix:
  // Column 0: defuzzified output,
  // Columns 1..totalRules: rule activations.
  NumericMatrix output(n, totalRules + 1);
  
  // Loop over each observation.
  for (int i = 0; i < n; i++) {
    // For each observation, first precompute clamped values for each variable.
    std::vector<double> clampedValues(nVars);
    for (int j = 0; j < nVars; j++) {
      double x = columns[j][i];
      if (hasLimits[j]) {
        if (x < lowerLimits[j]) x = lowerLimits[j];
        if (x > upperLimits[j]) x = upperLimits[j];
      }
      clampedValues[j] = x;
    }
    
    std::vector<double> ruleActivations(totalRules, 0.0);
    // For each rule (r from 0 to totalRules-1), determine the combination of MFs.
    for (int r = 0; r < totalRules; r++) {
      double activation = 1.0;
      int idx = r;
      // Loop over each variable.
      for (int j = 0; j < nVars; j++) {
        // Compute divisor = product of mVec for variables j+1 to nVars-1.
        int divisor = 1;
        for (int k = j + 1; k < nVars; k++) {
          divisor *= mVec[k];
        }
        // Determine the MF index for variable j (0-indexed).
        int mfIndex = idx / divisor;
        idx = idx % divisor;
        
        List varMFs = membershipFunctions[j];
        List mf = varMFs[mfIndex];
        std::string type = as<std::string>(mf["type"]);
        NumericVector params = mf["params"];
        // Use the precomputed clamped value.
        double x = clampedValues[j];
        double mu = computeMembership(x, type, params);
        activation *= mu;
      }
      ruleActivations[r] = activation;
    }
    
    // Compute the defuzzified output as the weighted average of the rule consequents.
    double weightedSum = 0.0, sumActivation = 0.0;
    for (int r = 0; r < totalRules; r++) {
      weightedSum += ruleActivations[r] * ruleConsequents[r];
      sumActivation += ruleActivations[r];
    }
    double finalOutput = (sumActivation > 0) ? weightedSum / sumActivation : 0.0;
    output(i, 0) = finalOutput;
    for (int r = 0; r < totalRules; r++) {
      output(i, r + 1) = ruleActivations[r];
    }
  }
  
  return output;
}
