library(dplyr)

# read gruner data
arth <- read.csv("inst/raw_data/arth.csv")

# add the full binomial for each spp
arth$species_binom <- paste(arth$genus, arth$species)

# read checklist dadta
arthropod_checklist <- read.csv("inst/raw_data/hawaii_arthropod_checklist.csv")

# new df, true/false statement for each island to tell us if species is found there
island_occ <- arthropod_checklist[,c("Kauai", "Oahu", "Molokai", "Lanai", 
                                    "Kahoolawe", "Maui", "HawaiiIsland")] != ""

# tells us how many islands the species is found on
num_islands <- rowSums(island_occ)

# create vector with non-endemic as default & fill with endemic status 
endemic_status <- rep("Non-Endemic", length(num_islands))

#grepl function searches for first argument in second argument,
#returns TRUE if found 

endemic_status[num_islands == 1 & 
                   grepl("end|ind", arthropod_checklist$Status)] <- "Endemic" 

endemic_status[num_islands == 0 ] <- "Unknown"

# Some Unknowns -> "Non-Endemic" if known to be non-native
endemic_status[!grepl("end|ind", 
                      arthropod_checklist$Status)] <- "Non-Endemic"


# adding single island endemic status column
arthropod_checklist$endemic_status <- endemic_status

# add species binomial
arthropod_checklist$species_binom <- paste(arthropod_checklist$Genus, 
                                           arthropod_checklist$Species)

# subset arthopod checklist 
arthropod_checklist <- arthropod_checklist[ , c("species_binom",
                                                "endemic_status")]


# correct spelling in arth

# column "bad_names" refer to names in gruner data aka `arth`
# column "bad_name_match" are names they should be changed to so they match
# with names in `arthropod_checklist`

name_corr <- read.csv("inst/raw_data/name_corrections.csv")


# indeces of names to updated in `arth`
ibad <- match(name_corr$bad_names, arth$species_binom)
arth$species_binom[ibad] <- name_corr$bad_name_match



# merge with gruner data
arth <- left_join(arth, arthropod_checklist, by = "species_binom", 
                  relationship = "many-to-many")

# change NA endemic status to unknown
arth$endemic_status[is.na(arth$endemic_status)] <- "Unknown"

# write out the data
save(arth, file = "data/arth.rda")
