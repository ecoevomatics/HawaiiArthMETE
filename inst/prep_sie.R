library(dplyr)
library(tidyr)

# read gruner data
arth <- read.csv("inst/raw_data/arth.csv")

# add the full binomial for each spp
arth$species_binom <- paste(arth$genus, arth$species)

# read checklist dadta
arthropod_checklist <- read.csv("inst/raw_data/hawaii_arthropod_checklist.csv")

# pivot checklist to long format with island occupancy as one column
chklist_lng <- pivot_longer(arthropod_checklist, Kauai:HawaiiIsland, 
                            names_to = "island") |> 
    filter(value %in% c("Oa", "Ma", "Ha", "Ka", 
                        "Mo", "Kh", "Ln")) |> 
    select(-value) |> 
    rename(species_binom = Genus.and.species) |> 
    group_by(species_binom) |> 
    mutate(num_islands = n()) |> 
    ungroup() |> 
    mutate(sie = num_islands == 1 & grepl("end", Status))

