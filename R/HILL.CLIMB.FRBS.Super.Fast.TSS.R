#' Train the fuzzy rule consequents using the hill climbing algorithm.
#'
#' @param Membership the matrix with the membership of each datum to each fuzzy rule obtained using the function FUZZIFY.FRBS.Fast.
#' @param Training.dataset the training datataset in data.frame format. The target species is expected to be named as "Species".
#' @param Trials number of repetitions to avoid getting stuck in a local minimum.
#'
#' @return list with the best performance (True Skill Statistic, TSS) and the optimal consequents consisting in a vector of ones and zeroes of length the number of fuzzy rules.
#' @export
#'
#' @examples
#' 
#' data(Lepomis.gibbosus.dataset)  
#' data(Lepomis.gibbosus.FRBS)
#' 
#' Memberships <- FUZZIFY.FRBS.Fast(Data =
#' Lepomis.gibbosus.dataset[,Lepomis.gibbosus.FRBS$ImpVariables],
#' FRBS = Lepomis.gibbosus.FRBS)
#' #'
#' Optimised.consequents <- HILL.CLIMB.FRBS.Super.Fast.TSS(Membership = Memberships,
#'                                                         Training.dataset = Lepomis.gibbosus.dataset,
#'                                                         Trials = 9)
#'
#'
HILL.CLIMB.FRBS.Super.Fast.TSS <- function(Membership, Training.dataset, Trials) {
  Optimal.Consequents.Matrix <- NULL
  TSSs <- NULL

  for (Trial in 1:Trials)
  {
    set.seed(Trial)
    Temporal.Consequents <- c(sample(c(0, 1), nrow(Membership), replace = T))

    Output.value <- apply(Membership, 2, function(x) {
      sum(c(x) * c(Temporal.Consequents)) / sum(x)
    })

    TSS <- sum(caret::confusionMatrix(data = cut(Output.value, breaks = c(0, 0.5, 1), labels = c(0, 1), include.lowest = T), reference = factor(Training.dataset[, "Species"], levels = c("0", "1")), positive = "1")$byClass[1:2]) - 1

    TSSRef <- TSS

    set.seed(Trial)
    RandomHillClimbing <- sample(c(1:nrow(Membership)))

    for (ii in 1:nrow(Membership))
    {
      Temporal.Consequents[which(RandomHillClimbing == ii)] <- c(0, 1)[which(c(0, 1) != Temporal.Consequents[which(RandomHillClimbing == ii)])]

      Output.value <- apply(Membership, 2, function(x) {
        sum(c(x) * c(Temporal.Consequents)) / sum(x)
      })

      TSS <- sum(caret::confusionMatrix(data = cut(Output.value, breaks = c(0, 0.5, 1), labels = c(0, 1), include.lowest = T), reference = factor(Training.dataset[, "Species"], levels = c("0", "1")), positive = "1")$byClass[1:2]) - 1

      if (TSSRef > TSS) {
        Temporal.Consequents[which(RandomHillClimbing == ii)] <- c(0, 1)[which(c(0, 1) != Temporal.Consequents[which(RandomHillClimbing == ii)])]
      }

      if (TSSRef < TSS) {
        TSSRef <- TSS
      }
    }

    Optimal.Consequents.Matrix <- cbind(Optimal.Consequents.Matrix, Temporal.Consequents)
    TSSs <- c(TSSs, TSSRef)
  }

  Output.value <- apply(Membership, 2, function(x) {
    sum(c(x) * c(Optimal.Consequents.Matrix[, which.max(TSSs)])) / sum(x)
  })
  return(list(TSS = max(TSSs), Consequents = c(Optimal.Consequents.Matrix[, which.max(TSSs)])))
}
