arth <- read.csv("data/arth.csv")
arth_2 <- read.csv("data/arth.csv")
#View(arth)

#facet grid of tree and biomass
ggplot(arth, aes(x = log_biomass)) +
         geom_histogram() + 
         facet_grid(rows = vars(stage))

ggplot(arth, aes(x = indivistageggplot(arth, aes(x = individual_biomass_g)) + 
  geom_histogram(bins = 50)

arth$log_biomass <- log(arth$individual_biomass_g)

ggplot(arth, aes(sample = log_biomass)) +
  geom_qq() + 
  geom_qq_line()

#probably not normal unless we trim the dataa bit


ggplot(arth, aes(x = log_biomass)) +
  geom_histogram() + 
  facet_grid(rows = vars(stage))


#dplyr time

#trophic level and biomass

trophic_time <- arth %>% select(trophic, log_biomass)

#trophic levels
ggplot(trophic_time, aes(x = log_biomass)) + 
  geom_histogram() +
  facet_grid(rows = vars(trophic))

#native vs invasive
ggplot(arth, aes(x = log_biomass)) + 
  geom_histogram() + 
  facet_grid(rows = vars(origin))

#what is unknown
unknown <- arth %>% filter(trophic == "unknown")

#proportion of trophic levels per site, does it change with 

#subset each tree 1-9

#for each of them calculate a sum of the biomass for each trophic level

#divide that sum by the total biomass at the site

#for site one, 
#prop_detritivores_site1 <- total biomass of detritivores / total biomass at site 1 (total biomass at the site or of sthe study)
#niche

#each dot = tree replicate 
#y axis = proportion of x niche? 
#x axis: levels of nitrogen/phos

#D, H , Pa, Pr, U  each trophic niche would 


                          
                          
library(ggplot2)
library(dplyr)
                        
# remember, I made up some fake data, the column names in your data
# will be different
                        
# make N to P ratio
arth$NtoP <- arth$N / arth$P
                        
# summarize by tree and site
dat <- group_by(dat, site, tree)
dat_summary <- summarize(dat,
NtoP = mean(NtoP),
tot_biomass = sum(biomass),
D_biomass = sum(biomass[diet == "D"]),
H_biomass = sum(biomass[diet == "H"]),
P_biomass = sum(biomass[diet == "P"]))
dat_summary <- ungroup(dat_summary)
                        
# make scatter plots
ggplot(dat_summary, aes(x = NtoP, y = D_biomass / tot_biomass)) +
geom_point()
                        
ggplot(dat_summary, aes(x = NtoP, y = H_biomass / tot_biomass)) +
geom_point()
                        
ggplot(dat_summary, aes(x = NtoP, y = P_biomass / tot_biomass)) +
geom_point()
                        
                        
For the second question about deviation from the MaxEnt model, this is a start:
library(meteR)
                        
# example for logLikZ
                        
data(arth)
                        
## object holding ecosystem structure function
esf1 <- meteESF(spp=arth$spp,
abund=arth$count,
power=arth$mass^(.75),
minE=min(arth$mass^(.75)))
## calculate species abundance distribution
ipd1 <- sad(esf1)
                        
## calculate z-score, keeping all simulated log likelihoods for plotting
llz <- logLikZ(ipd1, nrep=100, return.sim=TRUE)
                        
                # you just need the "z" value
                        llz$z
                        
                        # you would need to calculate this z for every tree for every site
                        