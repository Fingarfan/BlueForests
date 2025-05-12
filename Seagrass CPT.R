library(bnlearn)
###############################################################
###############Seagrass CPTS - Portugal########################
###############################################################

rm(list = ls()) #Clearing the environment
gc()
library(bnlearn) 

################################################################
##############SECTION 1 - DATA##################################
################################################################

######################################################
#######Probability matrix of Parent's nodes###########
######################################################

#Burial Activities
Burial <- matrix(c(0.435483871, 1-0.435483871),
                 byrow = TRUE, ncol = 2,
                 dimnames = list(NULL, c("High Burial", "Low Burial")))

#Extreme Events
E_Events <- matrix(c(0.330882353, 1-0.330882353),
                   byrow = TRUE, ncol = 2,
                   dimnames = list(NULL, c("High Extreme", "Low Extreme")))

#Alien Species (Prior)
Alien_S_Prior <- matrix(c(0.9, 0.1),
                        byrow = TRUE, ncol = 2,
                        dimnames = list(NULL, c("High Species","Low Species")))

#Clam Harvesting
Clam_H <- matrix(c(0.846428571, 1-0.846428571),
                  byrow = TRUE, ncol = 2,
                  dimnames = list(NULL, c("High Harvest", "Low Harvest")))

#Grazers (Prior)
Grazers_Prior <- matrix(c(0.604651163, 1-0.604651163),
                        byrow = TRUE, ncol = 2,
                        dimnames = list(NULL, c("High Grazers","Low Grazers")))

#Water Turbidity
Turbidity_Prior <- matrix(c(0.766666667, 1-0.766666667),
                    byrow = TRUE, ncol = 2,
                    dimnames = list(NULL, c("High Turbidity", "Low Turbidity")))

#Eutrophication
Eutrophication <- matrix(c(0.362903226, 1-0.362903226),
                  byrow = TRUE, ncol = 2,
                  dimnames = list(NULL, c("High Eutrophication", "Low Eutrophication")))

#Water Temperature
Temperature <- matrix(c(0.263333333, 0.5, 0.236666667),
                    byrow = TRUE, ncol = 3,
                    dimnames = list(NULL, c("High WT", "Medium WT", "Low WT")))

#Grazers (conditional on Water Temperature)
Grazers <- matrix(c(0.68, 0.54, 0.4, 0.32, 0.46, 0.6),
                  byrow = TRUE, ncol = 3,
                  dimnames = list(c("High Grazers","Low Grazers"), c("High WT", "Medium WT", "Low WT")))

#Alien Species (conditional on Water Temperature)
Alien_S <- matrix(c(0.905263158, 0.763157895, 0.525925926, 0.094736842, 0.236842105, 0.474074074),
                  byrow = TRUE, ncol = 3,
                  dimnames = list(c("High Species","Low Species"), c("High WT", "Medium WT", "Low WT")))

#Water Turbidity
Turbidity <- matrix(c(0.8, 0.566666667, 0.2, 0.433333333),
                  byrow = TRUE, ncol = 2,
                  dimnames = list(c("High Turbidity","Low Turbidity"), c("High WT", "Low WT")))

###########################################################
###Probability matrix of Seagrass given each parent node###
###########################################################

#Burial Activity
Seagrass.BA <- matrix(c(0.037096774, 0.185483871, 0.777419355, 
                        0.525806452, 0.325806452, 0.148387097),
                  byrow = FALSE, ncol = 2,
                  dimnames = list(c("Good Seagrass", "Average Seagrass", "Low Seagrass"), c("High Burial","Low Burial")))

#Extreme Events
Seagrass.EE <- matrix(c(0.194736842, 0.294736842, 0.510526316, 
                        0.631578947, 0.218421053, 0.15),
                  byrow = FALSE, ncol = 2,
                  dimnames = list(c("Good Seagrass", "Average Seagrass", "Low Seagrass"), c("High Extreme","Low Extreme")))

#Clam Harvesting
Seagrass.CH <- matrix(c(0.235, 0.3625, 0.4025,
                        0.515, 0.335, 0.15),
                      byrow = FALSE, ncol = 2,
                      dimnames = list(c("Good Seagrass", "Average Seagrass", "Low Seagrass"), c("High Harvesting","Low Harvesting")))

#Alien Species
Seagrass.AS <- matrix(c(0.145652174, 0.226086957, 0.62826087, 
                        0.514285714, 0.3, 0.185714286),
                  byrow = FALSE, ncol = 2,
                  dimnames = list(c("Good Seagrass", "Average Seagrass", "Low Seagrass"), c("High Species","Low Species")))

#Grazers
Seagrass.GRAZER <- matrix(c(0.1625, 0.3625, 0.475, 
                        0.44545, 0.345454545, 0.209090909),
                      byrow = FALSE, ncol = 2,
                      dimnames = list(c("Good Seagrass", "Average Seagrass", "Low Seagrass"), c("High Grazers","Low Grazers")))

#Water Turbidity
Seagrass.TURB <- matrix(c(0.108333333, 0.229166667, 0.6625, 
                          0.529166667, 0.283333333, 0.1875),
                    byrow = FALSE, ncol = 2,
                    dimnames = list(c("Good Seagrass", "Average Seagrass", "Low Seagrass"), c("High Turbidity","Low Turbidity")))

#Eutrophication
Seagrass.EUTRO <- matrix(c(0.106521739, 0.206521739, 0.686956522, 
                           0.558695652, 0.315217391, 0.126086957),
                        byrow = FALSE, ncol = 2,
                        dimnames = list(c("Good Seagrass", "Average Seagrass", "Low Seagrass"), c("High Eutrophication","Low Eutrophication")))

#Water Temperature
Seagrass.TEMP <- matrix(c(0.21, 0.346, 0.444, 
                      0.754, 0.132, 0.114, 
                      0.536, 0.264, 0.2),
                    byrow = FALSE, ncol = 3,
                    dimnames = list(c("Good Seagrass", "Average Seagrass", "Low Seagrass"), c("High WT", "Average WT","Low WT")))

#############################################################
###Probability matrix of Ecosystem Services given Seagrass###
#############################################################

#Biodiversity
Biodiversity <- matrix(c(0.766666667, 0.175757576, 0.057575758, 
                         0.575757576, 0.293939394, 0.13030303, 
                         0.227272727, 0.290909091, 0.481818182),
                       byrow = FALSE, ncol = 3,
                       dimnames = list(c("High Biodiversity", "Average Biodiversity", "Low Biodiversity"), c("Good Seagrass", "Average Seagrass", "Low Seagrass")))

#Nursery Areas
Nurs.Area <- matrix(c(0.80625, 0.15625, 0.0375, 
                      0.54063, 0.278125, 0.18125, 
                      0.075, 0.3125, 0.6125),
                    byrow = FALSE, ncol = 3,
                    dimnames = list(c("High Nursery", "Average Nursery", "Low Nursery"), c("Good Seagrass", "Average Seagrass", "Low Seagrass")))

#Carbon Sequestration
C.Seq <- matrix(c(0.682352941, 0.233823529, 0.083823529, 
                  0.55206, 0.243823529, 0.204117647, 
                  0.233823529, 0.310294118, 0.455882353),
                byrow = FALSE, ncol = 3,
                dimnames = list(c("High Sequestration", "Average Sequestration", "Low Sequestration"), c("Good Seagrass", "Average Seagrass", "Low Seagrass")))

#Erosion Control
Erosion.Control <- matrix(c(0.66969697, 0.251515152, 0.078787879, 
                            0.539393939, 0.327272727, 0.133333333,
                            0.263636364, 0.336363636, 0.4),
                      byrow = FALSE, ncol = 3,
                      dimnames = list(c("High Control", "Medium Control", "Low Control"), c("Good Seagrass", "Average Seagrass", "Low Seagrass")))

#Water Purification
Water.Pur <- matrix(c(0.734615385, 0.217307692, 0.048076923, 
                      0.469230769, 0.317307692, 0.213461538, 
                      0.305882353, 0.3, 0.394117647),
                    byrow = FALSE, ncol = 3,
                    dimnames = list(c("High Purification", "Average Purification", "Low Purification"), c("Good Seagrass", "Average Seagrass", "Low Seagrass")))

#Biological Control
Bio.Control <- matrix(c(0.746153846, 0.253846154, 
                        0.453846154, 0.546153846,
                       0.253846154, 0.746153846),
                     byrow = FALSE, ncol = 3,
                     dimnames = list(c("High Biological Control", "Low Biological Control"), c("Good Seagrass", "Average Seagrass", "Low Seagrass")))

#Emblematic Species
Emblematic <- matrix(c(0.591304348, 0.408695652, 
                        0.5, 0.5,
                       0.408695652, 0.591304348),
                      byrow = FALSE, ncol = 3,
                      dimnames = list(c("Non-threatened", "Threatened"), c("Good Seagrass", "Average Seagrass", "Low Seagrass")))


#####################################################################
##########################SECTION 2 - Setup##########################
#In this section we assume conditional independence between nodes ###
#This allows us to easily estimate the probability of Seagrass|nodes#
#####################################################################


#Extract the possible states for each parent node
Seagrass_states <- c("Good Seagrass", "Average Seagrass", "Low Seagrass")
burial_states <- c("High Burial", "Low Burial")
e_events_states <- c("High Extreme", "Low Extreme")
clam_harvest_states <-c("High Harvest", "Low Harvest")
alien_s_states <- c("High Species", "Low Species")
grazers_states <- c("High Grazers", "Low Grazers")
turbidity_states <- c("High Turbidity", "Low Turbidity")
eutrophication_states <- c("High Eutrophication", "Low Eutrophication")
temperature_states <- c("High WT", "Average WT", "Low WT")

#Creates all possible combinations of parent node states
parent_combinations <- expand.grid(Burial = burial_states,
                                   E_Events = e_events_states,
                                   Clam_H = clam_harvest_states,
                                   Alien_S = alien_s_states,
                                   Grazers = grazers_states,
                                   Turbidity = turbidity_states,
                                   Eutrophication = eutrophication_states,
                                   Temperature = temperature_states)
                                   

#Creates the Conditional Probability Table of Seagrass with 'null' values
cpt_Seagrass <- matrix(NA, nrow = length(Seagrass_states), ncol = nrow(parent_combinations),
                   dimnames = list(Seagrass_states, NULL))

#Sets the column names of the CPT to represent each combination of parent nodes
colnames(cpt_Seagrass) <- apply(parent_combinations, 1, paste, collapse = "_")

for (i in 1:nrow(parent_combinations)) {
  #Extracts the current combination of parent nodes
  current_combination <- parent_combinations[i, ]
  
  #Extracts the state of each parent node
  ba_state <- current_combination[["Burial"]]
  ee_state <- current_combination[["E_Events"]]
  ch_state <- current_combination[["Clam_H"]]
  as_state <- current_combination[["Alien_S"]]
  gr_state <- current_combination[["Grazers"]]
  turb_state <- current_combination[["Turbidity"]]
  eutro_state <- current_combination[["Eutrophication"]]
  temp_state <- current_combination[["Temperature"]]
  
  #Extracts the conditional probability matrices for each parent node
  probs_ba <- Seagrass.BA[, ba_state]
  probs_ee <- Seagrass.EE[, ee_state]
  probs_ch <- Seagrass.CH[, ch_state]
  probs_as <- Seagrass.AS[, as_state]
  probs_gr <- Seagrass.GRAZER[, gr_state]
  probs_turb <- Seagrass.TURB[, turb_state]
  probs_eutro <- Seagrass.EUTRO[, eutro_state]
  probs_temp <- Seagrass.TEMP[, temp_state]
  
  #Finds the probability of Seagrass independently conditional on parent nodes
  probs_combined <- probs_ba * probs_ee * probs_ch * probs_as * probs_gr * probs_turb * probs_eutro * probs_temp
  
  #Normalizes the probabilities to sum up to 1 across "Good Seagrass", "Average Seagrass", and "Low Seagrass"
  probs_normalized <- probs_combined / sum(probs_combined)
  
  #Assigns the normalized probabilities to the corresponding column in the CPT matrix
  cpt_Seagrass[, i] <- probs_normalized
}

dim(cpt_Seagrass) <- c(length(Seagrass_states), length(burial_states),
                   length(e_events_states), length(clam_harvest_states),
                   length(alien_s_states), length(grazers_states),
                   length(turbidity_states), length(eutrophication_states),
                   length(temperature_states))

dimnames(cpt_Seagrass) <- list(Seagrass = Seagrass_states, Burial = burial_states,
                          E_Events = e_events_states,
                               Clam_H = clam_harvest_states , Alien_S = alien_s_states,
                           Grazers = grazers_states, Turbidity = turbidity_states,
                           Eutrophication = eutrophication_states, 
                          Temperature = temperature_states)
                           
#Visualization Plot
cpt_Seagrass

#Exporting the data
options(max.print=1000000)
capture.output(print(cpt_Seagrass), file = "Seagrass_cpt.txt")





