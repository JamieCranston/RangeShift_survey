# Todo convert to ggplot
#' plot_supp_fig_4
#' @description produces faceted plot of respondent demographics
#' @param data cleaned respondent data table
#' @return faceted plot of respondent demographics
#' @export
#'
plot_supp_fig_4 <- function(data) {

  #   tiff(
  #   file = "Figures/SummaryDemographics.tif",
  #   width = 173,
  #   height = 260,
  #   res = 600,
  #   units = "mm"
  # )


  graphics::par(mfrow = c(3, 2))
  cex <- 1.2
  graphics::par(
    cex.lab = cex,
    cex.axis = cex,
    cex.main = cex
  )

  graphics::hist(data$age, col = "grey", main = "Age", xlab = "Age")
  graphics::hist(data$years_recording, col = "grey", main = "Years Recording", xlab = "Years Recording")
  graphics::plot(as.factor(data$gender))

  graphics::title("Gender")
  graphics::plot(data$education)
  graphics::title("Level of Education")
  graphics::plot(as.factor(data$wildlife_sector))
  graphics::title("Working in Wildlife Sector")
  graphics::plot(as.factor(data$involvement))
  graphics::title("Recording Role")
}
