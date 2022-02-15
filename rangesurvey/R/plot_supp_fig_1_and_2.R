#' plot_supp_fig_1
#'
#' @param data Survey data (cleaned)
#' @param config Config file (for paths to census  data)
#' @importFrom readxl read_xlsx
#' @return supplementary figure 1
#' @export
plot_supp_fig_1 <- function(data, config) {
  
  Census <- readxl::read_xlsx(
    config$census_dirs$sex,
    sheet = 3,
    skip = 12,
    n_max = 3,
    col_names = c("Sex", "Number")
  )

  Sex_Data <- data.frame(
    "Survey" = table(as.factor(data$gender))[2:1],
    "Census" = Census$Number
  )

  Sex_Prop_Model <- brms::brm(
    Sex ~ 1,
    data = data.frame(Sex = c(
      rep("Male", Sex_Data[which(Sex_Data$Survey.Var1 == "Male"), "Survey.Freq"]),
      rep("Female", Sex_Data[which(Sex_Data$Survey.Var1 ==
        "Female"), "Survey.Freq"])
    )),
    # prior = c(set_prior("normal(0.4895482,0.5)", class = "Intercept", dpar = "muMale")),
    family = brms::categorical()#,
    #sample_prior = TRUE,
    #cores = 4
  )
  
  posterior_male <- stats::density(stats::predict(Sex_Prop_Model,
                                summary = TRUE)[, 1])
  census_prop_male <- prop.table(Sex_Data$Census)[1]
  post_95ci_male <- stats::quantile(stats::predict(Sex_Prop_Model,
                                                   summary = TRUE)[, 1], c(0.025, 0.975))
  
  posterior_female <- stats::density(stats::predict(Sex_Prop_Model,
                                             summary = TRUE)[, 2])
  census_prop_female <- prop.table(Sex_Data$Census)[2]
  post_95ci_female <- stats::quantile(stats::predict(Sex_Prop_Model,
                                                   summary = TRUE)[, 2], c(0.025, 0.975))
  
  df <- dplyr::bind_rows(data.frame(P = posterior_male$x, density = posterior_male$y, gender = "male"),
                  data.frame(P = posterior_female$x, density = posterior_female$y, gender = "female")
  )
   
  supp_figure_1 <- ggplot(data = df) +
    geom_line(aes(x = P, y = density, colour = gender))+
    geom_vline(xintercept = post_95ci_male)+ # 95% male
    geom_vline(xintercept = post_95ci_female)+ # 95% female
    geom_vline(xintercept = census_prop_male, colour = "red")+ # UK census male
    geom_vline(xintercept = census_prop_female, colour = "blue") + # UK census female
  ggtitle("Reported sex amongst UK Census respondents (vlines)\nEstimates from our sample shown (density plots)")+
    xlab("Proportion")

  return(supp_figure_1)
}

#' plot_supp_fig_2
#'
#' @param data Survey data (cleaned)
#' @param config Config file (for paths to census  data)
#'
#' @return supplementary figure 2
#' @export
#'
plot_supp_fig_2 <- function(data, config) {
  Census_Ed <- readxl::read_xlsx(
    config$census_dirs$education,
    sheet = 3,
    skip = 12,
    n_max = 7,
    col_names = c("Ed_level", "Number")
  ) %>%
    dplyr::mutate(Survey_cats = dplyr::case_when(
      Ed_level == "Level 1 qualifications" | Ed_level == "Level 2 qualifications" ~ "GCSEs",
      Ed_level == "Level 3 qualifications" ~ "A-levels",
      Ed_level == "Level 4 qualifications and above" ~ "Undergrad",
    )) %>%
    stats::na.omit() %>%
    dplyr::group_by(.data$Survey_cats) %>%
    dplyr::summarise(n = sum(.data$Number))

  Ed_Data <- data.frame(
    "Survey" = table(forcats::fct_collapse(as.factor(data$education),
      Undergrad = c("Undergrad", "Postgrad")
    )),
    "Census" = Census_Ed$n[c(2,1,3)]
  )


  Ed_Prop_Model <- brms::brm(Education ~ 1,
    data.frame(Education = c(
      rep("A-levels", Ed_Data[which(Ed_Data$Survey.Var1 == "A-levels"), "Survey.Freq"]),
      rep("GCSEs", Ed_Data[which(Ed_Data$Survey.Var1 ==
        "GCSEs"), "Survey.Freq"]),
      rep("Undergrad", Ed_Data[which(Ed_Data$Survey.Var1 ==
        "Undergrad"), "Survey.Freq"])
    )),
    family = brms::categorical()
  )
  
  posterior_A_levels <- stats::density(stats::predict(Ed_Prop_Model,
                                                  summary = TRUE)[, "P(Y = A-levels)"])
  census_prop_A_levels <- prop.table(Ed_Data$Census)[2]
  post_95ci_A_levels <- stats::quantile(
    stats::predict(Ed_Prop_Model, summary = T)[, "P(Y = A-levels)"],
    c(0.025, 0.975)
  )  
  
  posterior_GCSEs <- stats::density(stats::predict(Ed_Prop_Model,
                                                    summary = TRUE)[, "P(Y = GCSEs)"])
  census_prop_GCSEs <- prop.table(Ed_Data$Census)[1]
  post_95ci_GCSEs <- stats::quantile(
    stats::predict(Ed_Prop_Model, summary = T)[, "P(Y = GCSEs)"],
    c(0.025, 0.975)
  )  
  
  posterior_undergrad <- stats::density(stats::predict(Ed_Prop_Model,
                                                    summary = TRUE)[, "P(Y = Undergrad)"])
  census_prop_undergrad <- prop.table(Ed_Data$Census)[3]
  post_95ci_undergrad <- stats::quantile(
    stats::predict(Ed_Prop_Model, summary = T)[, "P(Y = Undergrad)"],
    c(0.025, 0.975)
  )
    
  
  df <- dplyr::bind_rows(data.frame(P = posterior_A_levels$x,
                                    density = posterior_A_levels$y,
                                    Education = "A-levels"),
                         data.frame(P = posterior_GCSEs$x,
                                    density = posterior_GCSEs$y,
                                    Education = "GCSEs"),
                         data.frame(P = posterior_undergrad$x,
                                    density = posterior_undergrad$y,
                                    Education = "Undergrad")
  )
  
  
  supp_figure_2 <- ggplot(data = df) +
    geom_line(aes(x = P, y = density, colour = Education)) +
    geom_vline(xintercept = post_95ci_A_levels) + # 95%
    geom_vline(xintercept = post_95ci_GCSEs) + # 95%
    geom_vline(xintercept = post_95ci_undergrad) + # 95%
    geom_vline(xintercept = census_prop_A_levels, colour = "#FF4040") +
    geom_vline(xintercept = census_prop_GCSEs, colour = "green3") +
    geom_vline(xintercept = census_prop_undergrad, colour = "#00B2EE")+
    ggtitle("Proportion of highest attained qualification amongst UK Census respondents (vlines)
            \nEstimates from our sample shown (density plots)")+
    xlab("Proportion")


  # graphics::par(mfrow = c(1, 3))
  # plot(
  #   stats::density(stats::predict(Ed_Prop_Model, summary = T)[, "P(Y = A-levels)"]),
  #   xlim = c(0.1, 0.2),
  #   main = "P(respondent having at\nleast A-Level qualification)",
  #   xlab = "P"
  # )
  # graphics::abline(v = prop.table(Ed_Data$Census)[1], col = "red")
  # graphics::abline(
  #   v = stats::quantile(
  #     stats::predict(Ed_Prop_Model, summary = T)[, "P(Y = A-levels)"],
  #     c(0.025, 0.975)
  #   ),
  #   col = "blue"
  # )
  # plot(
  #   stats::density(stats::predict(Ed_Prop_Model, summary = T)[, "P(Y = GCSEs)"]),
  #   xlim = c(0.05, 0.45),
  #   main = "P(respondent having at\nleast GCSES qualification)",
  #   xlab = "P"
  # )
  # graphics::abline(v = prop.table(Ed_Data$Census)[1], col = "red")
  # graphics::abline(
  #   v = stats::quantile(
  #     stats::predict(Ed_Prop_Model, summary = T)[, "P(Y = GCSEs)"],
  #     c(0.025, 0.975)
  #   ),
  #   col = "blue"
  # )
  # plot(
  #   stats::density(stats::predict(Ed_Prop_Model, summary = T)[, "P(Y = Undergrad)"]),
  #   xlim = c(0.35, 0.9),
  #   main = "P(respondent having at\nleast undergraduate qualification)",
  #   xlab = "P"
  # )
  # graphics::abline(v = prop.table(Ed_Data$Census)[3], col = "red")
  # graphics::abline(
  #   v = stats::quantile(
  #     stats::predict(Ed_Prop_Model, summary = T)[, "P(Y = Undergrad)"],
  #     c(0.025, 0.975)
  #   ),
  #   col = "blue"
  # )
  return(supp_figure_2)
}
