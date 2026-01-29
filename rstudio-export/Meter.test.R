install.packages("meteR")
library(meteR)
?'meteR'
?'logLikZ'

#State variables 
    # A0 = area
    # S0 = total species in Area
    #N0 = number of individuals 
    # E0 total rate of metabolic energy use 

#ESF = ecological structure function: joint distribution of 
# metabolic rate and abundance 

#SSF = distribution of abundance over area 

#SAD = species abundance distribution 

#IPD= individual power distribution 
  #across community 

#SIPD = individual power distribution for a species with n individuals 

#SPD = distribution of  average metabolic rate (of individuals) over species 

#ebar = individual metabolic rate for species with n individuals

#SAR= species area relationship 
#EAR = endemics area relationships
  # endemics: species only found in one geographical location

#ESF -> sad -> calculate z scores 

#group by, summarize dplyr -> step 0 for each tree on each site 
 # 
#n() = how many rows in "sub data frame" from group by (arth,site, tree)



