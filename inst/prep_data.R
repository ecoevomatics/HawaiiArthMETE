#read in raw data 

arth <- read.csv("inst/raw_data/arth.csv")


#write a .rda file

save(arth, file = "data/arth.rda")

