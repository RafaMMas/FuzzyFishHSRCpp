#' Function to create a zero-order Takagi–Sugeno–Kang fuzzy rule-based systems (FRBS)
#'
#' @param ImpVariables the names of the input variables in vector format
#' @param Range a matrix with as many columns as input variables with the minimum and maximum expected values for each variable
#' @param MFfunction the selected membership function
#' @param MFparameters the parameters of the membership functions. It should be a matrix with as many columns as fuzzy sets. The columns for each variable must include the variable name as indicated in *ImpVariables*.
#' @param Consequents the zero-order TSK fuzzy rule consequents.
#'
#' @return a list containing the five components of the FRBSs
#' @export
#'
#' @examples
#' FRBS <- Create.FRBS.R(ImpVariables = c("Velocity",
#'                                        "Depth",
#'                                        "Substrate.index",
#'                                        "Cover.index"),
#'                       Range = matrix(c(0, 1.164, 0, 2.67, 0, 8, 0, 1), nrow = 2, byrow = FALSE,
#'                                      dimnames = list(NULL, c("Velocity",
#'                                                              "Depth",
#'                                                              "Substrate.index",
#'                                                              "Cover.index"))),
#'                       MFfunction = PIMF,
#'                       MFparameters = matrix(c(0, 0, 0.0377, 0.4,
#'                                               0.0377, 0.4, 1.164, 1.164,
#'                                               0, 0, 0.46, 0.8767,
#'                                               0.46, 0.8767, 1.3067, 2.67,
#'                                               1.3067, 2.67, 2.67, 2.67,
#'                                               0, 0, 0, 0.5333,
#'                                               0, 0.5333, 3.8667, 6,
#'                                               3.8667, 6, 8, 8,
#'                                               0, 0, 0, 1,
#'                                               0, 1, 1, 1), nrow = 4, byrow = FALSE,
#'                                      dimnames = list(NULL, c("VelocityL", "VelocityH",
#'                                             "DepthL", "DepthM", "DepthH",
#'                                             "Substrate.indexL", "Substrate.indexM",
#'                                             "Substrate.indexH",
#'                                             "Cover.indexL", "Cover.indexH"))),
#'                       Consequents = c(0, 0, 1, 0,
#'                       1, 0, 0, 0,
#'                       1, 0, 0, 0,
#'                       0, 0, 0, 0,
#'                       0, 0, 1, 0,
#'                       1, 1, 0, 0.5,
#'                       0, 0, 1, 0,
#'                       0, 1, 1, 0,
#'                       0, 0, 0, 0)
#'                       )
#'
Create.FRBS.R <- function(ImpVariables, Range, MFfunction, MFparameters, Consequents) {
  list(ImpVariables = ImpVariables, Range = Range, MFfunction = MFfunction, MFparameters = MFparameters, Consequents = Consequents)
}

#' Lepomis gibbosus Dataset  
#'  
#' This dataset contains four microhabitat variables: Velocity, Depth, Substrate, and Cover,  
#' along with the presence (1) or absence (0) of *Lepomis gibbosus*.  
#'  
#' @format A data frame with X rows and Y columns:  
#' \describe{  
#'   \item{Velocity}{Water velocity at the sampling site (units?)}  
#'   \item{Depth}{Water depth at the sampling site (units?)}  
#'   \item{Substrate}{Type of substrate present (categorical or numerical?)}  
#'   \item{Cover}{Presence of cover structures (categorical or numerical?)}  
#'   \item{Presence}{Binary indicator: 1 (present) or 0 (absent)}  
#' }  
#'  
#' @source Source of the dataset (e.g., study, publication, or fieldwork details).  
#' @name Lepomis.gibbosus.dataset  
#' @docType data  
#' @keywords datasets  
#' @usage data(Lepomis.gibbosus.dataset)  
NULL  

#' Optimal Fuzzy Rule-Based System (FRBS) Model for *Lepomis gibbosus*  
#'  
#' This object represents the optimal FRBS model developed for predicting the habitat suitability  
#' of *Lepomis gibbosus* based on key environmental variables.  
#'  
#' @name Lepomis.gibbosus.FRBS  
#' @docType data  
#' @keywords datasets  
#' @usage data(Lepomis.gibbosus.FRBS)  
NULL  
