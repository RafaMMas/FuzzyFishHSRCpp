#' Microhabitat use of *Lepomis gibbosus*
#'
#' This dataset contains environmental and habitat-related variables for species observations, including velocity, depth, and various cover types.
#'
#' @format A data frame with 11 variables:
#' 
#' \describe{
#'   \item{Species}{Binary variable indicating species presence (1) or absence (0).}
#'   \item{Velocity}{Water velocity (m/s).}
#'   \item{Depth}{Water depth (m).}
#'   \item{Substrate.index}{Index representing mean substrate type, ranging from 0 to 8.}
#'   \item{Caves}{Binary variable indicating the presence (1) or absence (0) of caves.}
#'   \item{Reeds}{Binary variable indicating the presence (1) or absence (0) of reeds.}
#'   \item{Aqu.veg.}{Binary variable indicating the presence (1) or absence (0) of aquatic vegetation.}
#'   \item{Shade}{Binary variable indicating the presence (1) or absence (0) of shade.}
#'   \item{Rocks}{Binary variable indicating the presence (1) or absence (0) of rocks.}
#'   \item{Wood.logs}{Binary variable indicating the presence (1) or absence (0) of wood logs.}
#'   \item{Cover.index}{Index representing the degree of cover, ranging from 0 to 1.}
#' }
#'
#' @usage data(Lepomis.gibbosus.dataset)
#' 
#' @references
#' Mu\ifelse{html}{\out{&ntilde;}}{\enc{UTF-8}{ñ}}oz-Mas, R., Macian-Sorribes, H., Oliva-Paterna, F. J.,
#' Sangelantoni, L., Peano, D., Pulido-Velazquez, M., & Mart\ifelse{html}{\out{&iacute;}}{\enc{UTF-8}{í}}nez-Capel, F. (2024).
#' Adaptation measures to global change in the Serpis River Basin (Spain): An evaluation considering agricultural benefits,
#' environmental flows, and invasive fishes. \emph{Ecological Indicators}, 161, 111979.
#' DOI: 10.1016/j.ecolind.2024.111979
#' 
#' @examples
#' data(Lepomis.gibbosus.dataset)
#' summary(Lepomis.gibbosus.dataset)
#'
"Lepomis.gibbosus.dataset"


#' Optimal Fuzzy Rule-Based System (FRBS) Model for *Lepomis gibbosus*  
#'  
#' This object represents the optimal FRBS model developed for predicting the habitat suitability  
#' of *Lepomis gibbosus* based velocity, depth, substrate and cover.  
#'  
#' @name Lepomis.gibbosus.FRBS
#'   
#' @docType list
#'   
#' @keywords datasets
#' 
#' @references
#' Mu\ifelse{html}{\out{&ntilde;}}{\enc{UTF-8}{ñ}}oz-Mas, R., Macian-Sorribes, H., Oliva-Paterna, F. J.,
#' Sangelantoni, L., Peano, D., Pulido-Velazquez, M., & Mart\ifelse{html}{\out{&iacute;}}{\enc{UTF-8}{í}}nez-Capel, F. (2024).
#' Adaptation measures to global change in the Serpis River Basin (Spain): An evaluation considering agricultural benefits,
#' environmental flows, and invasive fishes. \emph{Ecological Indicators}, 161, 111979.
#' DOI: 10.1016/j.ecolind.2024.111979
#'
#' @usage data(Lepomis.gibbosus.FRBS)  
NULL  
