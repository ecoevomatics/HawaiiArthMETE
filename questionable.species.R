library(dplyr)

#setup
load("data/arth.rda")



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


#subset arthopod checklist 
arthopod_checklist <- arthopod_checklist[,c("Genus","Species","endemic_status")]

# extract names in arth that donʻt match to arthropod_checklist
bad_names <- unique(arth$genus[!(arth$genus %in% arthopod_checklist$Genus)])
bad_names <- bad_names[!grepl("_g", bad_names)]



bad_name_match <- character(length(bad_names))

for(i in 1:length(bad_names)) {
  m <- agrep(bad_names[i], arthopod_checklist$Genus, value = TRUE)
  bad_name_match[i] <- paste(unique(m), collapse = ", ")
}

cbind(bad_names, bad_name_match)


#Plagiomeris appears as Plagiomerus (which exists..) when running agrep function. 
# but all "Plagiomeris", "Plagiolepis", and "Plagiomerus" appear as null for 
#arth status function -> assume non endemic? 

#Valenzuela seems to be correctly spelled. Found only in arth dataset? 
agrep("Valenzuela", arthopod_checklist$Genus, value = TRUE)

#Tetracnemoidea same as above. 
agrep("Tetracnemoidea", arthopod_checklist$Genus, value = TRUE)

# Greenidea '' 

#Libnotes '' 
#COLEOP '' 
#nr_Gastrancistrus ''
#Leiophron ''
#Signiphora ''
# Stenocaecilius''

# Rhopalum agrep result is Rhopalomyia, which seems like correct spelling 
# based on google
agrep("Rhopalum", arthopod_checklist$Genus, value = TRUE)

#Trimorus same as ''
# Haplophallus ''
#Tenuiphantes''
#Epuraea ''
#Asecodes ''



x <- 1:5
y <- numeric(length(x))

for(i in 1:length(x)) {
  y[i] <- 3 + 10 * x[i]
}

y
