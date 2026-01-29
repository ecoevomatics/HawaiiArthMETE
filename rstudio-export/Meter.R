# x = site, ordered by N:P ratio

#new plot site vs N:P 

#meteR testing 

library(meteR)
library(dplyr)
library(ggplot2)

arth <- new_arth_data

arth_meter <- group_by(arth, site, tree)
arth_meter <- summarize(new_arth_data,
                        llz = {
                          this_esf <- meteESF(spp = new_arth_data$species_code,
                                              abund = new_arth_data$count, rep(1, n()),
                                              power = new_arth_data$individual_biomass_g^0.75)
                          this_sad <- sad(this_esf)
                          
                          logLikZ(this_sad)$z
                        })
arth_meter <- ungroup(arth_meter)

# order sites by N:P
arth_meter$site <- factor(arth_meter$site, levels = c("VO", "LA", "KH",
                                                      "MO", "KA"))
# visualize
ggplot(arth_meter, aes(x = site, y = llz)) +
  geom_boxplot()