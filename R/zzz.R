#' Arthropod data
#' 
#' Data set from Gruner (2007) 
#'
#' @format A \code{data.frame} with 17,024 rows and 21 columns
#' \describe{
#'  \item{site}{two letter site code, a factor ordered by flow age}
#'  \item{site_name}{full site name, a factor ordered by flow age}
#'  \item{flow_age_my}{flow age in millions of years}
#'  \item{specimen_code}{unique identifier for specimen}
#'  \item{collection_code}{unique identifier for collection}
#'  \item{tree}{tree number}
#'  \item{tray}{tray number}
#'  \item{date_collected}{sampling date}
#'  \item{class}{taxonomic class}
#'  \item{order}{taxonomic order}
#'  \item{family}{taxonomic family}
#'  \item{species_code}{unique code for species ID}
#'  \item{genus}{taxonomic genus}
#'  \item{species}{taxonomic species}
#'  \item{trophic}{character code for trophic level}
#'  \item{origin}{status as native or non-native}
#'  \item{stage}{life stage}
#'  \item{lengthmm}{length in mm}
#'  \item{abundance}{abundance, all rows are single specimen, so aundance = 1}
#'  \item{ind_biom}{estimated biomass}
#'  \item{metabolic_rate}{ind_biom^(3/4)}
#' }
#' @name arth
#' @docType data
#' @references Gruner, D. S. (2007). Geological age, ecosystem development, 
#'     and local resource constraints on arthropod community structure in the 
#'     Hawaiian Islands. Biological Journal of the Linnean Society, 90(3), 
#'     551-570.
NULL
