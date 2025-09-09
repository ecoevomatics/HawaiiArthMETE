
#setup
load("data/arth.rda")

library(meteR)
library(ggplot2)
library(dplyr)
library(tidyverse)

arthopod_checklist<-read.csv("inst/raw_data/hawaii_arthropod_checklist.csv")

#new df, true/false statement for each island to tell us if species is found there
island_occ <- arthopod_checklist[,c("Kauai", "Oahu", "Molokai", "Lanai", 
                                    "Kahoolawe", "Maui", "HawaiiIsland")] != ""
#tells us how many islands the species is found on
num_islands <- rowSums(island_occ)

#create empty vector & fill with endemic status 
endemic_status <- character(length(num_islands))

#grepl function searches for first argument in second argument,
#returns TRUE if found 

endemic_status[num_islands == 1 & 
                 grepl("end|ind",arthopod_checklist$Status)] <- "Endemic" 

endemic_status[num_islands == 0 ] <- "Unknown"
#Some Unknowns -> "Non-Endemic" if known to be non-native
endemic_status[!grepl("end|ind", arthopod_checklist$Status)] <- 
  "Non-Endemic"

 
#adding status column
arthopod_checklist$endemic_status <- endemic_status
 View(arthopod_checklist)

#subset arthopod checklist 
arthopod_checklist <- arthopod_checklist[,c("Genus","Species","endemic_status")]


 #Finding what genera in arth don't match arthopod_checklist 
unique(arth$genus[!(arth$genus %in% arthopod_checklist$Genus)])

#correcting misspellings + check endemic status 
arth$status[arth$genus == "Valenzuela"]


# to figure out correct spelling
agrep("Valenzuela", arthopod_checklist$Genus,value = TRUE)
arth$genus[arth$genus == "Platosciara"] <- "Plastosciara"

arth$species[arth$genus == "Greenidea"]
"nr_Gastrancistrus" %in% arth$genus
"Leiophron" %in% arthopod_checklist$Genus

#joining df by genus and species
comb_data <- left_join(arth, arthopod_checklist, join_by(genus == Genus, 
                                                          species == Species))

comb_data$endemic_status
#empty strings in endemic_status column -> why?
