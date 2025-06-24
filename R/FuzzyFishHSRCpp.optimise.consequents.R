
Observed.output = DB[,4]
data = DB[,-4]
FRBS = New.FRBSs.type
case.weights = NULL
Max.value = 1
Min.value = NULL
Rescale = FALSE
case.weights = c(1/table(DB[,4]))[as.character(DB[,4])]

Weighted.mean.optimisation <- function(Observed.output,
                                       data,
                                       FRBS,
                                       case.weights = NULL,
                                       Max.value = 1,
                                       Min.value = NULL,
                                       Rescale = FALSE){
  
if(is.null(case.weights))
  {
    case.weights <- rep(1, nrow(data))
  }

Membership <- evaluateFIS_cpp(data,
                              membershipFunctions = FRBS[[1]],
                              ruleConsequents = rep(1, prod(unlist(lapply(FRBS[[1]], function(x){length(x)})))),
                              inputLimits = FRBS[[3]])[,-1]

Consequents <- apply(Membership, 2 , function(x)
  {
    stats::weighted.mean(x = Observed.output, w = x*case.weights)
  })

if(Rescale){
  Consequents <- (Consequents - min(Consequents))/(max(Consequents)-min(Consequents))
}

if(!is.null(Max.value)){
  Consequents[which.max(Consequents)] <- Max.value
}

if(!is.null(Min.value)){
  Consequents[which.min(Consequents)] <- Min.value
}

return(Consequents)
  
}

Consequents <- Weighted.mean.optimisation(Observed.output,
                                          data,
                                          FRBS,
                                          case.weights,
                                          Max.value = 1,
                                          Min.value = NULL,
                                          Rescale = FALSE)

