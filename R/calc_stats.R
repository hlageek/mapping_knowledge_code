#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param isvavai_data
#' @param people_top_profiles
#' @param pca_disc_ind
#' @return
#' @author hlageek
#' @export
calc_stats <- function(
          isvavai_data,
          people_top_profiles,
          pca_disc,
          pca_disc_ind,
          pca_projection,
          document_topic_matrix,
          axes_coordinates_table
          ) {

require(tidytable)

# Initialize list
stats <- list()

# Manually set entries

stats$nrow_topterms_df <- 10 # How many topics to list in a table?
attributes(stats$nrow_topterms_df)  <- list(
    description = "Number of topics presented in the table for each pole"
  )

stats$n_topterms <- 5 # How many terms to represent a topic in a table?
attributes(stats$n_topterms)  <- list(
    description = "Number of terms (maximum) that represent topics in the table"
  )

# Automatically calculated entries


stats$n_pubs <- arrow::read_feather(isvavai_data$riv_identifiers$path) |> 
select(id_unique) |> 
bind_rows(arrow::read_feather(document_topic_matrix) |> select(id_unique = doc)) |> 
filter(!is.na(id_unique)) |> 
summarise(n = n_distinct(id_unique)) |> 
pull(n)
attributes(stats$n_pubs)  <- list(
    description = "Number of publications in the main (RIV) dataset"
  )


stats$n_grants <- arrow::read_feather(isvavai_data$cep_details$path) |> 
select(kod) |> filter(!is.na(kod)) |> 
summarise(n = n_distinct(kod)) |> pull(n)
attributes(stats$n_grants)  <- list(
    description = "Number of unique grants in the grant (CEP) dataset"
  )



stats$n_authors <- arrow::read_feather(isvavai_data$riv_authors$path) |> 
select(vedidk) |> filter(!is.na(vedidk)) |> 
summarise(n = n_distinct(vedidk)) |> pull(n)
attributes(stats$n_authors)  <- list(
    description = "Number of distinct authors in the main (RIV) dataset"
  )

stats$n_pis <- arrow::read_feather(isvavai_data$cep_investigators$path) |> 
filter(role_researcher == "G" & role_institution == "primary") |> 
select(vedidk) |> filter(!is.na(vedidk)) |> 
summarise(n = n_distinct(vedidk)) |> pull(n)
attributes(stats$n_pis)  <- list(
    description = "Number of distinct authors in the main (RIV) dataset"
  )

topmodel <- arrow::read_feather(document_topic_matrix)

stats$n_top_pubs <-  topmodel |> 
select(doc) |> 
summarise(n = n_distinct(doc)) |> pull(n)
attributes(stats$n_top_pubs)  <- list(
    description = "Number of distinct publications in the topic model"
  )


stats$n_topics <- topmodel |> 
select(starts_with("topic")) |> 
colnames() |> 
length()
attributes(stats$n_topics)  <- list(
    description = "Number of topics in the topic model"
  )

stats$n_pcs <- pca_disc_ind |> 
select(starts_with("PC")) |> 
ncol()
attributes(stats$n_pcs)  <- list(
    description = "Number of principal components, which corresponds to the number of disciplines"
  )


eigs <- pca_disc$sdev^2
helper_pca_stats <- tidytable::tidytable(
 sd = sqrt(eigs),
 proportion = eigs/sum(eigs),
 cumulative = cumsum(eigs)/sum(eigs)
 ) 

stats$component_var_1 <- round(helper_pca_stats$proportion[1]*100,1)
attributes(stats$component_var_1)  <- list(
    description = "Percentage of explained variance for PC1"
)

stats$component_var_2 <- round(helper_pca_stats$proportion[2]*100,1)
attributes(stats$component_var_2)  <- list(
    description = "Percentage of explained variance for PC2"
)

stats$component_var_3 <- round(helper_pca_stats$proportion[3]*100,1)
attributes(stats$component_var_3)  <- list(
    description = "Percentage of explained variance for PC3"
)

stats$cumulative_var_3 <- stats$component_var_1 + stats$component_var_2 + stats$component_var_3
attributes(stats$cumulative_var_3)  <- list(
    description = "Cumulative percentage of explained variance by first three PCs"
)

stats$pcs_interpreted <- axes_coordinates_table[["_row_groups"]] |> length()
attributes(stats$pcs_interpreted )  <- list(
    description = "Number of PCs retained for interpretation"
)

stats$n_top_individuals <- nrow(people_top_profiles)
attributes(stats$n_top_individuals)  <- list(
    description = "Number of authors with a topic profile"
)

stats$n_top_individuals_2 <- pca_projection |> 
filter(pubs_total >= 2) |> 
nrow()
attributes(stats$n_top_individuals_2)  <- list(
    description = "Number of authors with a topic profile and 2 or more publications"
)

stats$n_top_individuals_3 <- pca_projection |> 
filter(pubs_total > 2) |> 
nrow()
attributes(stats$n_top_individuals_3)  <- list(
    description = "Number of authors with a topic profile and more than 2 publications"
)

stats$n_top_individuals_5 <- pca_projection |> 
filter(pubs_total >= 5) |> 
nrow()
attributes(stats$n_top_individuals_5)  <- list(
    description = "Number of authors with a topic profile and 5 or more publications"
)

stats$n_top_individuals_6 <- pca_projection |> 
filter(pubs_total > 5) |> 
nrow()
attributes(stats$n_top_individuals_6)  <- list(
    description = "Number of authors with a topic profile and more then 5 publications"
)

stats


}
