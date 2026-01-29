#Time for some R and Plini :sunglasses:


library(dplyr) 
library(ggplot2)
library(meteR)

#data
arth <- read.csv("data/arth.csv")
arth_2 <- arth[,c("site", "site_name", "island","foliar_N", "foliar_P", "tree", "class", "order",
                         "family",  "genus", "species", "species_code", "trophic", "individual_biomass_g")]

#omit NA values
arth_2 <- na.omit(arth_2)
  #changes nothing :^)

##########################

#Andy's example code

# example for logLikZ

#example data, for some reason always replaces arth so.. be careful
#dat <- data(arth)

## object holding ecosystem structure function
#esf1 <- meteESF(spp=arth$spp,
#                abund=arth$count,
#                power=arth$mass^(.75),
#                minE=min(arth$mass^(.75)))
  #so, within meteESF he is defining species, count, power as ^0.75 of mass, 
  #minE as the minimum value of arth$mass^0.75

## calculate species abundance distribution
#ipd1 <- sad(esf1)
  #i am sad now

## calculate z-score, keeping all simulated log likelihoods for plotting
#llz <- logLikZ(ipd1, nrep=100, return.sim=TRUE)

# you just need the "z" value
#llz$z
  #the Z score for df = 2 of this data is 0.151062...

#Ok, it would be great if the lists were legible
#Perhaps the documentation could shed some light on this

################## some notes from the documentation

#It seems like this documentation has its own plotting infrastructure... 
#maybe ggplot not compatible here

#logLikZ() = log likelihood Z score, object class logLikZ


#sad() = species abundance distribution, object class SAD I think. 
        #print works on this object, returning a useful summary


#meteESF = joint distribution over abundance, n, and metabolic rate, e
  #Returns an object class of meteESF
  #meteR also provides tools to estimate the z-score of model ﬁt 
  #using either the log LikZ ormseZ functions.

###########################

#Alright, let's give this a try using the real arth data

#So, we need to  get this z score per tree per site
#We also need species counts per site:
#Likely this will entail group_by(), ungroup() and some other dplyr magic
#for now, just to get this code manipulable so we can learn from it together 
#as I have never seen this stuff before, I'll just add a column of ones
arth_2$count <- 1 
  #DEFINITELY fact check this with andy, but it seems to be the same concept 
  #in the example he gave

#object holding ecosystem structure function information
esf1 <- meteESF(spp=arth_2$species,
                abund=arth_2$count,
                power=arth_2$individual_biomass_g^(.75),
                minE=min(arth_2$individual_biomass_g^(.75)))
  #pause here and print(esf1)

#species abundance distribution 
ipd1 <- sad(esf1)


# calculate z-score, keeping all simulated log likelihoods for plotting
llz <- logLikZ(ipd1, nrep=100, return.sim=TRUE)

# you just need the "z" value
llz$z

#plot abundance vs Rank
plot(ipd1, ptype="rad", log="y")
  #Oh this is pretty cool, shows you the maximum entropy theory of ecology 
  #estimate of abundance compared to this model
  #Rank... 
  #OK so, meteR has its own plotting stuff, ggplot2 probably wont work
  #looks like it uses the base R plot() function
  #?plot() to read up on that, its basically simplified ggplot2
  #this plot looks surprisingly cool



#################################################

#SPAGHETTI CODE WARNING: USING SAME VARIABLES AS ABOVE CODE BECAUSE I AM LAZY

#This uh... creates a 700,000 obs long dataframe. Not sure I did it correctly.

#This code is definitely not correct in a couple ways

#What you need to do is figure out how to subset the data efficiently per tree per site CORRECTLY

#Then make sure with Andy about whether or not you need to sum biomass and species counts on tree/site basis

#if so, how to do that

#once you have all of that out of the way, follow the below pipeline to produce:
  #z score and plot of data and METE abundance vs Rank plots to... show
  #data conformation to model!

#plots accompanied by z scores would be interesting to see

#as for your hypothesis tests... well you have z scores, 
#maybe null distribution hypothesis testing is appropriate?
  #would be a great question for Andy. 

##################################################

#Now to do this per tree per site...
arth_grouped <- arth %>% group_by(tree, site) %>% 
                reframe(spp=arth_2$species,
                    abund=arth_2$count,
                    power=arth_2$individual_biomass_g^(.75),
                    minE=min(arth_2$individual_biomass_g^(.75)))
                    #R asked me to use reframe instead of summarise, why not. 

#grouped data into esf1... 
esf1 <- meteESF(spp=arth_grouped$spp,  
                abund=arth_grouped$abund,
                power=arth_grouped$power,
                minE=min(arth_grouped$minE))
#pause here and print(esf1) for fun


#continue the pipeline andy set up...
#species abundance distribution 
ipd1 <- sad(esf1)


# calculate z-score, keeping all simulated log likelihoods for plotting
llz <- logLikZ(ipd1, nrep=100, return.sim=TRUE)

# you just need the "z" value
llz$z
  #39.4555, interpret later

#plot abundance vs Rank
plot(ipd1, ptype="rad", log="y")                                                            
  #this plot shows METE line not conforming to data. Neat!                                                           
