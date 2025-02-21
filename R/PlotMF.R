#' Function to plot the membership functions
#'
#' @param FRBS the list compiling the elements describing a zero-order Takagi–Sugeno–Kang fuzzy rule-based system.
#' @param n.pt number of points/resolution used to plot each membership function.
#' @param data the training datataset in *data.frame* format.
#' @param Title the figure title.
#'
#' @return a 4 by 4 plot of the membership functions for each input variable
#' @importFrom graphics hist par title
#' @importFrom grDevices adjustcolor
#' @export
#'
#' @examples
#' 
#' data(Lepomis.gibbosus.dataset)  
#' data(Lepomis.gibbosus.FRBS)  
#' 
#' PlotMF(FRBS = Lepomis.gibbosus.FRBS, n.pt = 9999,
#' data = NULL,
#' Title = "Lepomis gibbosus")
#'
#' PlotMF(FRBS = Lepomis.gibbosus.FRBS, n.pt = 9999,
#' data = Lepomis.gibbosus.dataset,
#' Title = "Lepomis gibbosus")
#'
PlotMF <- function(FRBS, n.pt = 9999, data = NULL, Title = NULL) {
  Nrow <- 1
  Ncol <- 4
  op <- par(mfrow = c(Nrow, Ncol), oma = c(0.05, 0.05, 1.75, 0.05), mar = c(3.5, 3.5, 0.25, 0.25), mgp = c(2.25, 0.75, 0), cex.axis = 0.85, lwd = 0.5)
  for (i in FRBS$ImpVariables)
  {
    MFparameters <- FRBS$MFparameters[, stringr::str_detect(string = colnames(FRBS$MFparameters), pattern = i)]
    if (!is.null(data)) {
      h <- hist(data[which(data[, "Species"] == 0), i], breaks = seq(0, max(data[, i]), length = ifelse(i == "Cover.index", 7, 10)), plot = F)
      h$counts <- h$counts / sum(h$counts)
      plot(h, freq = TRUE, col = adjustcolor("grey", alpha.f = 0.5), border = "grey30", ylim = c(0, 1), xlab = "", ylab = "", main = "", axes = F)
      par(new = T)

      h <- hist(data[which(data[, "Species"] == 1), i], breaks = seq(0, max(data[, i]), length = ifelse(i == "Cover.index", 7, 10)), plot = F)
      h$counts <- h$counts / sum(h$counts)
      plot(h, freq = TRUE, col = adjustcolor("deeppink", alpha.f = 0.35), border = "deeppink4", ylim = c(0, 1), xlab = "", ylab = "", main = "", axes = F)
      par(new = T)
    }
    for (ii in 1:ncol(MFparameters))
    {
      plot(seq(min(FRBS$Range[, i]), max(FRBS$Range[, i]), length = n.pt),
        FRBS$MFfunction(pattern = seq(min(FRBS$Range[, i]), max(FRBS$Range[, i]), length = n.pt), parameters = MFparameters[, ii]),
        xlab = c("Velocity (m/s)", "Depth (m)", "Substrate index (-)", "Cover index (-)")[FRBS$ImpVariables == i], ylab = "Memb. & Freq.",
        xlim = range(FRBS$Range[, i]), ylim = c(0, 1),
        type = "l", col = "black", bty = "n",
        lwd = 1, las = 1
      )
      par(new = T)
    }
    par(new = F)
  }
  title(main = Title, outer = TRUE, font = 4)
  par(op)
}
