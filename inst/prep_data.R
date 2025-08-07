#read in raw data 

arth <- read.csv("inst/raw_data/arth.csv")

#convert sites to factors ordered by age 

arth$site <- factor(arth$site,levels = c("VO","LA","KH", "MO", "KA" ))

#write a .rda file

save(arth, file = "data/arth.rda")

