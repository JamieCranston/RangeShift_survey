SpecLookup <- read.csv("SpeciesLookup.csv")
SpecLookup$Name <- as.character(SpecLookup$Name)
SpecLookup$ID <- as.character(SpecLookup$ID )
FinalResp <- list()

Responses$VertEq1 <- as.character(Responses$VertEq1) 
Responses$VertEq2 <- as.character(Responses$VertEq2)
Responses$InvertEq1 <- as.character(Responses$InvertEq1)
Responses$InvertEq2 <- as.character(Responses$InvertEq2)

pivot_survey_long <- function(data) {
  
  vert_names <- c(
    "Egarzetta",
    "Pleucorodia",
    "Aalba",
    "Bibis",
    "Hhimantopus",
    "Pfalcinellus",
    "Iminutus"  ,
    "Apurpurea"
  )
  invert_names <- c(
    "Eviridulum",
    "Eornata",
    "Dsaxonica",
    "Aaffinis",
    "Rnebulosa",
    "Pkadenii",
    "Calgae",
    "Bhypnorum"
  )
  
  data <- data %>%
    dplyr::mutate(VertEq1 = cut(
      VertEq1,
      breaks = 8,
      labels = vert_names),
      VertEq2 = cut(
        VertEq2,
        breaks = 8,
        labels = vert_names),
      InvertEq1 = cut(
        InvertEq2,
        breaks = 8,
        labels = invert_names),
      InvertEq2 = cut(
        InvertEq2,
        breaks = 8,
        labels = invert_names)
    )
    

  data <- data %>%
    tidyr::pivot_longer(cols = contains("vertEq"), names_to = "Species")
  return(data)
}



for (rows in 1:nrow(Responses)) {
  FocalRow <- Responses[rows, ]
  Responses[rows, "VertEq1"] <- as.character(SpecLookup[which(SpecLookup$ID == FocalRow$VertEq1), "Name"])
  Responses[rows, "VertEq2"] <- as.character(SpecLookup[which(SpecLookup$ID == FocalRow$VertEq2), "Name"])
  Responses[rows, "InvertEq1"] <- as.character(SpecLookup[which(SpecLookup$ID == (as.numeric(FocalRow$InvertEq1) + 8)), "Name"])
  Responses[rows, "InvertEq2"] <- as.character(SpecLookup[which(SpecLookup$ID == (as.numeric(FocalRow$InvertEq2) + 8)), "Name"])
  
  V1 <- dplyr::select(FocalRow, contains(as.character(Responses[rows, "VertEq1"])))[-2]
  names(V1) <- c("Seen", "Attitude", "AttOpinion", "Remove", "Mitigate", "Accept", "Adapt", "Support", "ActOpinion")
  V2 <- dplyr::select(FocalRow, contains(as.character(Responses[rows, "VertEq2"])))[-2]
  names(V2) <- c("Seen", "Attitude", "AttOpinion", "Remove", "Mitigate", "Accept", "Adapt", "Support", "ActOpinion")
  I1 <- dplyr::select(FocalRow, contains(as.character(Responses[rows, "InvertEq1"])))[-2]
  names(I1) <- c("Seen", "Attitude", "AttOpinion", "Remove", "Mitigate", "Accept", "Adapt", "Support", "ActOpinion")
  I2 <- dplyr::select(FocalRow, contains(as.character(Responses[rows, "InvertEq2"])))[-2]
  names(I2) <- c("Seen", "Attitude", "AttOpinion", "Remove", "Mitigate", "Accept", "Adapt", "Support", "ActOpinion")
  TableSpecies <- rbind(V1, V2, I1, I2)
  FocalRow <- dplyr::select(
    FocalRow, -contains("Egarzetta"),
    -contains("Pleucorodia"),
    -contains("Aalba"),
    -contains("Bibis"),
    -contains("Hhimantopus"),
    -contains("Pfalcinellus"),
    -contains("Iminutus"),
    -contains("Apurpurea"),
    -contains("Eviridulum"),
    -contains("Eornata"),
    -contains("Dsaxonica"),
    -contains("Aaffinis"),
    -contains("Rnebulosa"),
    -contains("Pkadenii"),
    -contains("Calgae"),
    -contains("Bhypnorum")
  )
  
  FocalRow <- FocalRow[rep(seq_len(nrow(FocalRow)), each = 4), ]
  FinalResp[[rows]] <- cbind(FocalRow, TableSpecies)
  rm(V1, V2, I1, I2, FocalRow, TableSpecies)
}

Responses <- do.call("rbind", FinalResp)

rm(FinalResp, rows)
