#Todo convert to ggplot
#' plot_supp_fig_4
#' @description
#' @param data
#' @return
#' @export
#'
plot_supp_fig_4 <- function(data = respondent_table_clean){
  
#   tiff(
#   file = "Figures/SummaryDemographics.tif",
#   width = 173,
#   height = 260,
#   res = 600,
#   units = "mm"
# )

  
par(mfrow = c(3, 2))
cex <- 1.2
par(cex.lab = cex,
    cex.axis = cex,
    cex.main = cex)

hist(data$age, col = "grey", main = "Age", xlab = "Age")
hist(data$years_recording, col = "grey", main = "Years Recording", xlab = "Years Recording")
plot(as.factor(data$gender))

title("Gender")
plot(data$education)
title("Level of Education")
plot(as.factor(data$wildlife_sector))
title("Working in Wildlife Sector")
plot(as.factor(data$involvement))
title("Recording Role")

}