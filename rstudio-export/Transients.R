  arth <- read.csv("data/arth.csv")
new_arth_data <- arth[,c("site", "site_name", "island","foliar_N", "foliar_P", "tree", "class", "order",
                         "family",  "genus", "species", "species_code", "trophic", "individual_biomass_g")]

library(dplyr)
new_arth_data

#nitrogen to phos ratio 

new_arth_data$nitro_phos_ratio <- new_arth_data$foliar_N/new_arth_data$foliar_P

new_arth_data <- group_by(new_arth_data, site, tree)
new_arth_data_summary <- summarize(new_arth_data, nitro_phos_ratio = mean(nitro_phos_ratio), 
                                   total_biomass = sum(individual_biomass_g), 
                                   detritivore_biomass = sum(individual_biomass_g[trophic == "detritivore"]), 
                                   herbivore_biomass = sum(individual_biomass_g[trophic == "herbivore"]), 
                                   predator_biomass = sum(individual_biomass_g[trophic == "predator"]),
                                  transient_biomass = sum(individual_biomass_g[trophic == "transient"]),
                                  unknown_biomass = sum(individual_biomass_g[trophic == "unknown"]))
new_arth_data_summary <- ungroup(new_arth_data_summary)


transient_plot <- ggplot(new_arth_data_summary, aes(x= nitro_phos_ratio, y = transient_biomass/total_biomass)) + geom_jitter() +
  theme_classic() + labs(title= "Transients Biomass Proportion vs Resource Availability", x= 
                        "Ratio of Nitrogen to Phosphorus", y= "Transient Biomass/Total Biomass")

transient_plot

#qqplot
#biomass
qqplot <- ggplot(new_arth_data_summary, aes(sample=transient_biomass/total_biomass)) +
         geom_qq() +  geom_qq_line(alpha= 0.5, linewidth =2) + theme_classic() +labs(x="Theoretical quantiles", y="Observed quantiles")
qqplot
#n to p 
ggplot(new_arth_data_summary, aes(sample = nitro_phos_ratio) )+
geom_qq()+
  geom_qq_line(alpha =0.5, linewidth =2)

#transform qqplots + data
new_arth_data_summary$log_transient_biomass <- log(new_arth_data_summary$transient_biomass/new_arth_data_summary$total_biomass +1)
new_arth_data_summary$log_nitro_phos_ratio <- log(new_arth_data_summary$nitro_phos_ratio)

ggplot(new_arth_data_summary, aes(sample=log_transient_biomass))+
  geom_qq() +
  geom_qq_line(alpha= 0.5, linewidth = 2, color = "red") + theme_classic() +
  labs(x = "Theoretical Quantiles", y = "Observed Quantiles")
#more linear!, add red to make contrast clearer, remove grid background for clarity

ggplot(new_arth_data_summary, aes(sample = log_nitro_phos_ratio))+
  geom_qq() +
  geom_qq_line(alpha = 0.5, linewidth = 2) #transformation didn't change much


##post-transformation visualization 
ggplot(new_arth_data_summary, aes(x =log_nitro_phos_ratio, y =log_transient_biomass)) +
  geom_point(aes(colour= factor(log_nitro_phos_ratio)), size =3)+
  theme_classic()+
  labs(title = "Transient Biomass Proportion vs Resource Availabilty", x= "Ratio of Nitrogen to Phosphorus", y= "Transient Biomass/Total Biomass",
       colour = "Site")+ scale_color_discrete(labels = c("KH", "MO", "LA", "KA", "VO"))



#correlation test 
cor(new_arth_data_summary$log_nitro_phos_ratio, new_arth_data_summary$log_transient_biomass)
cor.test(new_arth_data_summary$log_nitro_phos_ratio, new_arth_data_summary$log_transient_biomass)
#NaN??

#add 1 to transformed biomass data

#t= -3.6656, df =43, p value = 0.0006737
#95CI = (-0.6835673, -0.2269028)
#reject the null 