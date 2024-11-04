#' Parse pinboard
#'
#' Parse pinboard stored on CESNET OwnCloud storage from public share link tokens.
#'
#' @title
#' @param base_url
#' @param mid_url
#' @param token
#' @param ext
#' @return
#' @author hlageek
#' @export

### PCA interpretation

reshape_pca_projection <- function(x, y) {
  x |> 
    select(vedidk, PC1, PC2, PC3, PC4, PC5) |> 
    inner_join(y) |> 
    na.omit() |> 
    mutate(
    percentile_axis1 = dplyr::ntile(PC1, 100),
    percentile_axis2 = dplyr::ntile(PC2, 100),
    percentile_axis3 = dplyr::ntile(PC3, 100)
    )  |> 
    pivot_longer(starts_with("percentile"), names_to = "axis", values_to = "percentile")  |> 
    mutate(axis = factor(axis, levels = c(
      "percentile_axis1",
      "percentile_axis2",
      "percentile_axis3"
      )))
}

pca_labels <- function() {
c(
      "Culture (0) - Nature (100)", 
      "Life (0) - Non-life (100)", 
      "Materials (0) - Methods (100)"
  ) 
}

customPalette <- function() {
c("#7E103C", "#146899", "#88800f", "#009E73", "#D55E00", "#50bbfa")
}
okabePalette <- function() {
c("#E69F00", "#56B4E9", "#009E73", "#0072B2", "#D55E00", "#CC79A7")
}
npgPalette <- function() {
c("#0072B2", "#E69F00", "#009E73", "#CC79A7", "#56B4E9", "#F0E442", "#D55E00", "#7E103C")
}


pca_labs_x <- function() { "PC coordinates percentile" }
pca_labs_color <- function() { "Axis" }

npgTheme <- function() {
theme_minimal(base_size = 14, base_family = "Arial") +
  theme(
    plot.background = element_rect(fill = "white"),
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(color = "black", linewidth = 1.3),
    axis.title.y = ggtext::element_markdown(),
    legend.position = "right",
    axis.text = element_text(size = 14),
    plot.title = element_text(face = "bold"),
    text = element_text(color = "black")
  )
}






### abbreviate disciplines

abbrev_discs <- function(discipline) {
  discipline_abbr <- tidytable::case_when(
    discipline == "Agricultural biotechnology" ~ "AgriBiotech",
    discipline == "Agriculture, Forestry, and Fisheries" ~ "AgriForeFish",
    discipline == "Animal and Dairy science" ~ "AniDairySci",
    discipline == "Arts (arts, history of arts, performing arts, music)" ~ "Arts",
    discipline == "Basic medicine" ~ "BasicMed",
    discipline == "Biological sciences" ~ "BioSci",
    discipline == "Chemical engineering" ~ "ChemEng",
    discipline == "Chemical sciences" ~ "ChemSci",
    discipline == "Civil engineering" ~ "CivEng",
    discipline == "Clinical medicine" ~ "ClinMed",
    discipline == "Computer and information sciences" ~ "CompInfoSci",
    discipline == "Earth and related environmental sciences" ~ "EarthEnvSci",
    discipline == "Economics and Business" ~ "EconBus",
    discipline == "Education" ~ "Edu",
    discipline == "Electrical eng., Electronic eng., Information eng." ~ "ElectrInfoEng",
    discipline == "Environmental biotechnology" ~ "EnvBiotech",
    discipline == "Environmental engineering" ~ "EnvEng",
    discipline == "Health sciences" ~ "HealthSci",
    discipline == "History and Archaeology" ~ "HistArch",
    discipline == "Industrial biotechnology" ~ "IndustryBiotech",
    discipline == "Languages and Literature" ~ "LangLit",
    discipline == "Law" ~ "Law",
    discipline == "Materials engineering" ~ "MaterialEng",
    discipline == "Mathematics" ~ "Math",
    discipline == "Mechanical engineering" ~ "MechanicalEng",
    discipline == "Media and communications" ~ "MediaComm",
    discipline == "Medical biotechnology" ~ "MedBiotech",
    discipline == "Medical engineering" ~ "MedEng",
    discipline == "Nano-technology" ~ "NanoTech",
    discipline == "Other Humanities and the Arts" ~ "OtherHumArts",
    discipline == "Other agricultural sciences" ~ "OtherAgriSci",
    discipline == "Other engineering and technologies" ~ "OtherEngTech",
    discipline == "Other medical sciences" ~ "OtherMedSci",
    discipline == "Other natural sciences" ~ "OtherNatSci",
    discipline == "Other social sciences" ~ "OtherSocSci",
    discipline == "Philosophy, Ethics and Religion" ~ "PhilEthRel",
    discipline == "Physical sciences" ~ "PhysicalSci",
    discipline == "Political science" ~ "PoliticalSci",
    discipline == "Psychology and cognitive sciences" ~ "PsychCogSci",
    discipline == "Social and economic geography" ~ "SocEcoGeo",
    discipline == "Sociology" ~ "Sociology",
    discipline == "Veterinary science" ~ "VeterinarySci",
    TRUE ~ discipline
  )
}
