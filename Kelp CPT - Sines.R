###############################################################
#####################KELP CPTS - Sines#########################
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

#Bottom Trawling
B_Trawling <- matrix(c(0.1, 0.9),
                     byrow = TRUE, ncol = 2,
                     dimnames = list(NULL, c("High BT", "Low BT")))

#Burial Activities
Burial <- matrix(c(0.38, 1-0.38),
                 byrow = TRUE, ncol = 2,
                 dimnames = list(NULL, c("High Burial", "Low Burial")))

#Extreme Events
E_Events <- matrix(c(0.7, 0.3),
                   byrow = TRUE, ncol = 2,
                   dimnames = list(NULL, c("High Extreme", "Low Extreme")))

#Infectious Diseases
Diseases <- matrix(c(0.5, 0.5),
                   byrow = TRUE, ncol = 2,
                   dimnames = list(NULL, c("High Diseases", "Low Diseases")))

#Water Turbidity
Turbidity <- matrix(c(0.5, 0.5),
                    byrow = TRUE, ncol = 2,
                    dimnames = list(NULL, c("High Turbidity", "Low Turbidity")))

#Water Temperature
Temperature <- matrix(c(0.165625, 0.590625, 0.24375),
                      byrow = TRUE, ncol = 3,
                      dimnames = list(NULL, c("High WT", "Medium WT", "Low WT")))

#Grazers (conditional on Water Temperature)
Grazers <- matrix(c(0.829166666666667, 0.741666666666667, 0.45, 
                    0.170833333333333, 0.258333333333333, 0.55),
                  byrow = TRUE, ncol = 3,
                  dimnames = list(c("High Grazers","Low Grazers"), c("High WT", "Medium WT", "Low WT")))

#Grazers (Prior)
Grazers_Prior <- matrix(c(0.9, 0.1),
                        byrow = TRUE, ncol = 2,
                        dimnames = list(NULL, c("High Grazers","Low Grazers")))

#Alien Species (conditional on Water Temperature)
Alien_S <- matrix(c(0.826666666666667, 0.826666666666667, 0.826666666666667, 
                    0.173333333333333, 0.173333333333333, 0.173333333333333),
                  byrow = TRUE, ncol = 3,
                  dimnames = list(c("High Species","Low Species"), c("High WT", "Medium WT", "Low WT")))

#Alien Species (Prior)
Alien_S_Prior <- matrix(c(0.9, 0.1),
                        byrow = TRUE, ncol = 2,
                        dimnames = list(NULL, c("High Species","Low Species")))


#######################################################
###Probability matrix of Kelp given each parent node###
#######################################################

#Bottom Trawling
Kelp.BT <- matrix(c(0.218181818181818, 0.136363636363636, 0.645454545454546, 
                    0.218181818181818, 0.2, 0.581818181818182),
                  byrow = FALSE, ncol = 2,
                  dimnames = list(c("Good Kelp", "Average Kelp", "Low Kelp"), c("High BT","Low BT")))

#Burial Activities
Kelp.BA <- matrix(c(0.192307692307692, 0.176923076923077, 0.630769230769231, 
                    0.192307692307692, 0.238461538461538, 0.569230769230769),
                  byrow = FALSE, ncol = 2,
                  dimnames = list(c("Good Kelp", "Average Kelp", "Low Kelp"), c("High Burial","Low Burial")))

#Extreme Events
Kelp.EE <- matrix(c(0.0933333333333333, 0.193333333333333, 0.713333333333333, 
                    0.28, 0.246666666666667, 0.473333333333333),
                  byrow = FALSE, ncol = 2,
                  dimnames = list(c("Good Kelp", "Average Kelp", "Low Kelp"), c("High Extreme","Low Extreme")))

#Alien Species
Kelp.AS <- matrix(c(0.1, 0.2, 0.7, 
                    0.1, 0.25, 0.65),
                  byrow = FALSE, ncol = 2,
                  dimnames = list(c("Good Kelp", "Average Kelp", "Low Kelp"), c("High Species","Low Species")))

#Infectious Diseases
Kelp.ID <- matrix(c(0, 0.2, 0.8, 
                    0, 0.3, 0.7),
                  byrow = FALSE, ncol = 2,
                  dimnames = list(c("Good Kelp", "Average Kelp", "Low Kelp"), c("High Diseases","Low Diseases")))

#Grazers
Kelp.GRAZER <- matrix(c(0.0230769230769231, 0.2, 0.776923076923077, 
                        0.0923076923076923, 0.3, 0.607692307692308),
                      byrow = FALSE, ncol = 2,
                      dimnames = list(c("Good Kelp", "Average Kelp", "Low Kelp"), c("High Grazers","Low Grazers")))

#Water Turbidity
Kelp.TURB <- matrix(c(0.138461538461538, 0.338461538461538, 0.523076923076923, 
                      0.230769230769231, 0.346153846153846, 0.423076923076923),
                    byrow = FALSE, ncol = 2,
                    dimnames = list(c("Good Kelp", "Average Kelp", "Low Kelp"), c("High Turbidity","Low Turbidity")))

#Water Temperature
Kelp.TEMP <- matrix(c(0.0233333333333333, 0.0933333333333333, 0.883333333333333, 
                      0.0933333333333333, 0.2, 0.706666666666667, 
                      0.14, 0.253333333333333, 0.606666666666667),
                    byrow = FALSE, ncol = 3,
                    dimnames = list(c("Good Kelp", "Average Kelp", "Low Kelp"), c("High WT", "Average WT","Low WT")))

#########################################################
###Probability matrix of Ecosystem Services given Kelp###
#########################################################

#Biodiversity
Biodiversity <- matrix(c( 0.584615384615385, 0.315384615384615, 0.1, 
                          0.353846153846154, 0.307692307692308, 0.338461538461538, 
                          0.207692307692308, 0.353846153846154,0.438461538461538),
                       byrow = FALSE, ncol = 3,
                       dimnames = list(c("High Biodiversity", "Average Biodiversity", "Low Biodiversity"), c("Good Kelp", "Average Kelp", "Low Kelp")))

#Nursery Area
Nurs.Area <- matrix(c(0.55, 0.3, 0.15, 
                      0.35, 0.3, 0.35, 
                      0.2, 0.35, 0.45),
                    byrow = FALSE, ncol = 3,
                    dimnames = list(c("High Nursery", "Average Nursery", "Low Nursery"), c("Good Kelp", "Average Kelp", "Low Kelp")))

#Carbon Sequestration
C.Seq <- matrix(c(0.604166666666667, 0.316666666666667, 0.0791666666666667, 
                  0.2, 0.416666666666667, 0.383333333333333, 
                  0.0791666666666667, 0.316666666666667, 0.604166666666667),
                byrow = FALSE, ncol = 3,
                dimnames = list(c("High Sequestration", "Average Sequestration", "Low Sequestration"), c("Good Kelp", "Average Kelp", "Low Kelp")))

#Water Purification
Water.Pur <- matrix(c(0.65, 0.25, 0.1, 
                      0.5, 0.2, 0.3, 
                      0.1, 0.225, 0.675),
                    byrow = FALSE, ncol = 3,
                    dimnames = list(c("High Purification", "Average Purification", "Low Purification"), c("Good Kelp", "Average Kelp", "Low Kelp")))

#Aquaculture
Aquaculture <- matrix(c(0.257692307692308, 0.742307692307692, 
                        0.207692307692308, 0.792307692307692,
                        0.0769230769230769, 0.923076923076923),
                      byrow = FALSE, ncol = 3,
                      dimnames = list(c("High Aquaculture", "Low Aquaculture"), c("Good Kelp", "Average Kelp", "Low Kelp")))

#Health Benefits
Health.Ben <- matrix(c(0.35, 0.65,
                       0.3, 0.7,
                       0.1, 0.9),
                     byrow = FALSE, ncol = 3,
                     dimnames = list(c("High Health Benefits", "Low Health Benefits"), c("Good Kelp", "Average Kelp", "Low Kelp")))


#####################################################################
##########################SECTION 2 - Setup##########################
#In this section we assume conditional independence between nodes ###
#This allows us to easily estimate the probability of Seagrass|nodes#
#####################################################################


#Extract the possible states for each parent node
kelp_states <- c("Good Kelp", "Average Kelp", "Low Kelp")
alien_s_states <- c("High Species", "Low Species")
burial_states <- c("High Burial", "Low Burial")
b_trawling_states <- c("High BT", "Low BT")
e_events_states <- c("High Extreme", "Low Extreme")
grazers_states <- c("High Grazers", "Low Grazers")
diseases_states <- c("High Diseases", "Low Diseases")
temperature_states <- c("High WT", "Average WT", "Low WT")
turbidity_states <- c("High Turbidity", "Low Turbidity")

#Creates all possible combinations of parent node states
parent_combinations <- expand.grid(Alien_S = alien_s_states,
                                   Grazers = grazers_states,
                                   B_Trawling = b_trawling_states,
                                   Burial = burial_states,
                                   Diseases = diseases_states,
                                   E_Events = e_events_states,
                                   Temperature = temperature_states,
                                   Turbidity = turbidity_states)

#Creates the Conditional Probability Table of Kelp with 'null' values
cpt_kelp <- matrix(NA, nrow = length(kelp_states), ncol = nrow(parent_combinations),
                   dimnames = list(kelp_states, NULL))

#Sets the column names of the CPT to represent each combination of parent nodes
colnames(cpt_kelp) <- apply(parent_combinations, 1, paste, collapse = "_")

for (i in 1:nrow(parent_combinations)) {
  #Extracts the current combination of parent nodes
  current_combination <- parent_combinations[i, ]
  
  #Extracts the state of each parent node
  as_state <- current_combination[["Alien_S"]]
  ba_state <- current_combination[["Burial"]]
  bt_state <- current_combination[["B_Trawling"]]
  ee_state <- current_combination[["E_Events"]]
  gr_state <- current_combination[["Grazers"]]
  id_state <- current_combination[["Diseases"]]
  temp_state <- current_combination[["Temperature"]]
  turb_state <- current_combination[["Turbidity"]]
  
  #Extracts the conditional probability matrices for each parent node
  probs_as <- Kelp.AS[, as_state]
  probs_ba <- Kelp.BA[, ba_state]
  probs_bt <- Kelp.BT[, bt_state]
  probs_ee <- Kelp.EE[, ee_state]
  probs_gr <- Kelp.GRAZER[, gr_state]
  probs_id <- Kelp.ID[, id_state]
  probs_temp <- Kelp.TEMP[, temp_state]
  probs_turb <- Kelp.TURB[, turb_state]
  
  #Finds the probability of Kelp independently conditional on parent nodes
  probs_combined <- probs_as * probs_ba * probs_bt * probs_ee * probs_gr * probs_id * probs_temp * probs_turb
  
  
  #Normalizes the probabilities to sum up to 1 across "Good Kelp", "Average Kelp", and "Low Kelp"
  probs_normalized <- probs_combined / sum(probs_combined)
  
  #Assigns the normalized probabilities to the corresponding column in the CPT matrix
  cpt_kelp[, i] <- probs_normalized
}

dim(cpt_kelp) <- c(length(kelp_states), length(alien_s_states),
                   length(burial_states), length(b_trawling_states),
                   length(e_events_states), length(grazers_states),
                   length(diseases_states), length(temperature_states),
                   length(turbidity_states))

dimnames(cpt_kelp) <- list(Kelp = kelp_states, Alien_S = alien_s_states,
                           Burial = burial_states, B_Trawling = b_trawling_states,
                           E_Events = e_events_states, Grazers = grazers_states,
                           Diseases = diseases_states, Temperature = temperature_states,
                           Turbidity = turbidity_states)

#Visualization Plot
cpt_kelp

#Exporting the data
options(max.print=1000000)
capture.output(print(cpt_kelp), file = "kelp_cpt_sines.txt")
