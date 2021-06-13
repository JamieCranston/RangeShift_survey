#' plot_supp_fig_1
#'
#' @param data 
#' @param config 
#'
#' @return
#' @export
plot_supp_fig_1 <- function(data, config) {
  
  Census <- readxl::read_xlsx(
    config$census_dirs$sex,
    sheet = 3,
    skip = 12,
    n_max = 3,
    col_names = c("Sex", "Number")
  )
  
  Sex_Data <-data.frame("Survey"= table(as.factor(data$gender))[2:1],
                        "Census" = Census$Number)
  
  Sex_Prop_Model <- brms::brm(
    Sex ~ 1,
    data = data.frame(Sex = c(
      rep("Male", Sex_Data[which(Sex_Data$Survey.Var1 == "Male"), "Survey.Freq"]),
      rep("Female", Sex_Data[which(Sex_Data$Survey.Var1 ==
                                     "Female"), "Survey.Freq"])
    )),
    #prior = c(set_prior("normal(0.4895482,0.5)", class = "Intercept", dpar = "muMale")),
    family = brms::categorical(),
    sample_prior = TRUE,
    cores = 4
  )
  
  par(mfrow = c(1, 3))
  
  plot(
    density(predict(Sex_Prop_Model, summary = T)[, 1]),
    xlim = c(0.3, 0.5),
    main = "P(respondent\nbeing female)",
    xlab = "P"
  )
  abline(v = prop.table(Sex_Data$Census)[1], col = "red")
  abline(v = quantile(predict(Sex_Prop_Model, summary = T)[, 1], c(0.025, 0.975)), col =
           "blue")
  plot(
    density(predict(Sex_Prop_Model, summary = T)[, 2]),
    xlim = c(0.5, 0.7),
    main = "P(respondent\nbeing male)",
    xlab = "P"
  )
  abline(v = prop.table(Sex_Data$Census)[2], col = "red")
  abline(v = quantile(predict(Sex_Prop_Model, summary = T)[, 2], c(0.025, 0.975)), col =
           "blue")
  
  return()
}

#' plot_supp_fig_2
#'
#' @param data 
#' @param config 
#'
#' @return
#' @export
#'
plot_supp_fig_2 <- function(data, config) {
  
  Census_Ed <- readxl::read_xlsx(
    config$census_dirs$education,
    sheet = 3,
    skip = 12,
    n_max = 7,
    col_names = c("Ed_level", "Number")
  )%>%
    dplyr::mutate(Survey_cats = dplyr::case_when(Ed_level == "Level 1 qualifications"|Ed_level == "Level 2 qualifications"~ "GCSEs",
                                   Ed_level == "Level 3 qualifications" ~ "A-levels",
                                   Ed_level == "Level 4 qualifications and above" ~ "Undergrad",
    ))%>%
    na.omit()%>%
    dplyr::group_by(Survey_cats)%>%
    dplyr::summarise(n = sum(Number))
  
  Ed_Data <- data.frame("Survey"= table(forcats::fct_collapse(as.factor(data$education),
                                                     Undergrad = c("Undergrad","Postgrad"))),
                        "Census" = Census_Ed$n)
  
  
  Ed_Prop_Model <- brms::brm(Education ~ 1,
                             data.frame(Education = c(
                               rep("A-levels", Ed_Data[which(Ed_Data$Survey.Var1 == "A-levels"), "Survey.Freq"]),
                               rep("GCSEs", Ed_Data[which(Ed_Data$Survey.Var1 ==
                                                            "GCSEs"), "Survey.Freq"]),
                               rep("Undergrad", Ed_Data[which(Ed_Data$Survey.Var1 ==
                                                                "Undergrad"), "Survey.Freq"])
                             )),
                             family = brms::categorical())
  
  par(mfrow = c(1, 3))
  plot(
    density(predict(Ed_Prop_Model, summary = T)[, "P(Y = A-levels)"]),
    xlim = c(0.1, 0.2),
    main = "P(respondent having at\nleast A-Level qualification)",
    xlab = "P"
  )
  abline(v = prop.table(Ed_Data$Census)[1], col = "red")
  abline(v = quantile(predict(Ed_Prop_Model, summary = T)[, "P(Y = A-levels)"],
                      c(0.025, 0.975)),
         col = "blue")
  plot(
    density(predict(Ed_Prop_Model, summary = T)[, "P(Y = GCSEs)"]),
    xlim = c(0.05, 0.45),
    main = "P(respondent having at\nleast GCSES qualification)",
    xlab = "P"
  )
  abline(v = prop.table(Ed_Data$Census)[1], col = "red")
  abline(v = quantile(predict(Ed_Prop_Model, summary = T)[, "P(Y = GCSEs)"],
                      c(0.025, 0.975)),
         col = "blue")
  plot(
    density(predict(Ed_Prop_Model, summary = T)[, "P(Y = Undergrad)"]),
    xlim = c(0.35, 0.9),
    main = "P(respondent having at\nleast undergraduate qualification)",
    xlab = "P"
  )
  abline(v = prop.table(Ed_Data$Census)[3], col = "red")
  abline(v = quantile(predict(Ed_Prop_Model, summary = T)[, "P(Y = Undergrad)"],
                      c(0.025, 0.975)),
         col = "blue")
}
