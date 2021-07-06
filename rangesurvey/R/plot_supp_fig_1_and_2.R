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
    family = brms::categorical(),
    sample_prior = TRUE,
    cores = 4
  )

  graphics::par(mfrow = c(1, 3))

  plot(
    stats::density(stats::predict(Sex_Prop_Model, summary = T)[, 1]),
    xlim = c(0.3, 0.5),
    main = "P(respondent\nbeing female)",
    xlab = "P"
  )
  graphics::abline(v = prop.table(Sex_Data$Census)[1], col = "red")
  graphics::abline(
    v = stats::quantile(stats::predict(Sex_Prop_Model, summary = T)[, 1], c(0.025, 0.975)), col =
      "blue"
  )
  plot(
    stats::density(stats::predict(Sex_Prop_Model, summary = T)[, 2]),
    xlim = c(0.5, 0.7),
    main = "P(respondent\nbeing male)",
    xlab = "P"
  )
  graphics::abline(v = prop.table(Sex_Data$Census)[2], col = "red")
  graphics::abline(
    v = stats::quantile(stats::predict(Sex_Prop_Model, summary = T)[, 2], c(0.025, 0.975)), col =
      "blue"
  )

  return()
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
    "Census" = Census_Ed$n
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

  graphics::par(mfrow = c(1, 3))
  plot(
    stats::density(stats::predict(Ed_Prop_Model, summary = T)[, "P(Y = A-levels)"]),
    xlim = c(0.1, 0.2),
    main = "P(respondent having at\nleast A-Level qualification)",
    xlab = "P"
  )
  graphics::abline(v = prop.table(Ed_Data$Census)[1], col = "red")
  graphics::abline(
    v = stats::quantile(
      stats::predict(Ed_Prop_Model, summary = T)[, "P(Y = A-levels)"],
      c(0.025, 0.975)
    ),
    col = "blue"
  )
  plot(
    stats::density(stats::predict(Ed_Prop_Model, summary = T)[, "P(Y = GCSEs)"]),
    xlim = c(0.05, 0.45),
    main = "P(respondent having at\nleast GCSES qualification)",
    xlab = "P"
  )
  graphics::abline(v = prop.table(Ed_Data$Census)[1], col = "red")
  graphics::abline(
    v = stats::quantile(
      stats::predict(Ed_Prop_Model, summary = T)[, "P(Y = GCSEs)"],
      c(0.025, 0.975)
    ),
    col = "blue"
  )
  plot(
    stats::density(stats::predict(Ed_Prop_Model, summary = T)[, "P(Y = Undergrad)"]),
    xlim = c(0.35, 0.9),
    main = "P(respondent having at\nleast undergraduate qualification)",
    xlab = "P"
  )
  graphics::abline(v = prop.table(Ed_Data$Census)[3], col = "red")
  graphics::abline(
    v = stats::quantile(
      stats::predict(Ed_Prop_Model, summary = T)[, "P(Y = Undergrad)"],
      c(0.025, 0.975)
    ),
    col = "blue"
  )
}
