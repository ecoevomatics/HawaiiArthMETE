#Andy's code: 

#library(meteR)
#library(dplyr)
#library(ggplot2)

# arth <- read.csv("arth.csv")

# add a column of 1's for abundance
#arth$count <- 1

# add column for metabolic power
#arth$metab_power <- arth$individual_biomass_g^0.75


#arth_meter <- group_by(arth, site, tree)


#arth_meter <- summarize(arth_meter,
                        
                        #llz = {
                          #this_esf <- meteESF(spp = species_code,
                                             # abund = count,
                                             # power = metab_power)
                          
                         # this_sad <- sad(this_esf)
                          
                        #  logLikZ(this_sad)$z
                      #  })
#arth_meter <- ungroup(arth_meter)

# order sites by N:P
#arth_meter$site <- factor(arth_meter$site, levels = c("VO", "LA", "KH",
                                                    #  "MO", "KA"))
# visualize
#ggplot(arth_meter, aes(x = site, y = llz)) +
 # geom_boxplot()

# null hypothesis test
#llz_mod <- lm(llz ~ site, data = arth_meter)
#anova(llz_mod)

#Attempt 
library(meteR)
library(ggplot2)
library(dplyr)

#subset data
arth <- read.csv("data/arth.csv")
arth_2 <-  arth[,c("site", "site_name", "island","foliar_N", "foliar_P", "tree", "class", "order", "flow_age_my",
                         "family",  "genus", "species", "species_code", "trophic", "individual_biomass_g")]

#create columns for abundance + metabolic rate 
arth_2$abundance <- 1
arth_2$metabolic_rate <- arth_2$individual_biomass_g^0.75

#group dataeset by site and tree
arth_2_meter <- group_by(arth_2, site, tree)
arth_2_meter <- summarise(arth_2_meter, 
                          llz = {
                            arth_esf <- meteESF(spp = species_code,
                                                 abund = abundance,
                                                 power = metabolic_rate)
                            
                            arth_sad <- sad(arth_esf)
                            logLikZ(arth_sad)$z #only need z value
                            
                          }) # works!! 

arth_2_meter <- ungroup(arth_2_meter)
arth_2_meter$site <- factor(arth_2_meter$site, levels = c("VO","LA","KH", "MO", "KA" ))
#Vo, LA, KH, MO, KA 

# site to N:P ratio 
arth_2$nitro_phos_ratio <- arth_2$foliar_N/arth_2$foliar_P
ggplot(arth_2, aes(x= site, y =nitro_phos_ratio))+ geom_point() +theme_classic()


#resource availability across sites (based on flow age)
np <- group_by(arth_2, site, tree) |>  #group levels of phos and nitro 
                                      #based on site
  summarize(foliar_N = mean(foliar_N), foliar_P = mean(foliar_P)) |>
  ungroup()

np$site <- factor(np$site, levels = c("VO", "LA", "KH", #level based on flow age
                                      "MO", "KA"))

ggplot(np, aes(x = site, y = foliar_N)) + 
  geom_point() + 
  labs(title= "Nitrogen availability across sites", x= "Site", y="Foliar Nitrogen") +
  theme_classic() 

ggplot(np, aes(x = site, y = foliar_P)) + 
  geom_point() + 
  labs(title= "Phosphorus availability across sites", x= "Site", y="Foliar Phosphorus") +
  theme_classic()


#visualization of data 
arth_2_llz_vs_site_plot <- ggplot(arth_2_meter, aes(x = site, y = llz)) +
geom_boxplot() + theme_classic() 
arth_2_llz_vs_site_plot
#higher z -> more different from ecological null? 

# null hypothesis test
llz_mod <- lm(llz ~ site, data = arth_2_meter)
View(llz_mod)
4.64488 # measures difference in means between two or more groups 
#f value - 4.6488
# P-value: 0.003549 
# reject the null distribution


#post hoc test 
aov_llz_mod <- aov(llz_mod)
TukeyHSD(aov_llz_mod)


# Null model plot attempt 
arth_2_grouped <- arth_2 %>% group_by(tree,site) %>% #group
  reframe(spp = arth_2$species_code,
          abund = arth_2$abundance,
          power = arth_2$metabolic_rate, 
          minE = min(arth_2$metabolic_rate))
#ecosystem structure function 
esf_2 <-meteESF(spp= arth_2_grouped$spp,
                abund= arth_2_grouped$abund,
                power=arth_2_grouped$power,
                minE = arth_2_grouped$minE)
print(esf_2) #S0= 605, N0= 719865 E0=15727857
arth_sad_2 <- sad(esf_2)
llz <- logLikZ(arth_sad_2, nrep = 100, return.sim = TRUE)

#z value 
llz$z #50.48977, df= 2 

#abundance v rank 
plot(arth_sad_2,ptype="rad", log="y")
?plot
