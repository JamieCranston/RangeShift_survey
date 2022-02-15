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
  
  age_plot <- ggplot(data)+
    geom_histogram(aes(age), binwidth = 4,  fill = "grey", colour = "black")+
    theme_classic()
  
  graphics::hist(data$years_recording, col = "grey", main = "Years Recording", xlab = "Years Recording")
  
  years_recording_plot <- ggplot(data)+
    geom_histogram(aes(years_recording), binwidth = 4, fill = "grey", colour = "black")+
    xlab("Years recording") +
    theme_classic()
  
  graphics::plot(as.factor(data$gender))
  graphics::title("Gender")
  
  gender_plot <- ggplot(data)+
    geom_bar(aes(as.factor(gender)))+
    xlab("Gender") +
    theme_classic()

  
  graphics::plot(data$education)
  graphics::title("Level of Education")
  
  education_plot <- ggplot(data %>%
                             dplyr::filter(!is.na(.data$education))
  ) +
    geom_bar(aes(education))+
    xlab("Education") +
    theme_classic()
  
  graphics::plot(as.factor(data$wildlife_sector))
  graphics::title("Working in Wildlife Sector")
  
  wildlife_sector_plot <- ggplot(data)+
    geom_bar(aes(wildlife_sector))+
    theme_classic()+
    xlab("Working in\nWildlife Sector")
  
  graphics::plot(as.factor(data$involvement))
  graphics::title("Recording Role")
  
  involvement_plot <- ggplot(data %>%
                               dplyr::filter(!is.na(.data$involvement)) %>% 
                               dplyr::mutate(involvement = forcats::fct_recode(as.factor(.data$involvement),"Informal\nrecording" = "recorder_informal",
                                                   "Formal\nrecording" = "recorder_formal",
                                                   "Verifying\nrecords" = "verifier",
                                                   "Organising\nrecording" = "organiser")
                               )
  ) +
    geom_bar(aes(involvement))+
    theme_classic() +
    xlab("Recording Role")
  
  arranged_plot <- age_plot +
    years_recording_plot +
    gender_plot +
    education_plot +
    wildlife_sector_plot +
    involvement_plot +
    patchwork::plot_layout(ncol = 2, nrow = 3)
  
  return(arranged_plot)
}


