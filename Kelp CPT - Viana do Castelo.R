###############################################################
###############KELP CPTS - Viana Do Castelo####################
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
B_Trawling <- matrix(c(0.196, 1-0.196),
                  byrow = TRUE, ncol = 2,
                  dimnames = list(NULL, c("High BT", "Low BT")))

#Burial Activities
Burial <- matrix(c(0.404347826, 1- 0.404347826),
                          byrow = TRUE, ncol = 2,
                          dimnames = list(NULL, c("High Burial", "Low Burial")))

#Extreme Events
E_Events <- matrix(c(0.760576923, 1- 0.760576923),
                 byrow = TRUE, ncol = 2,
                 dimnames = list(NULL, c("High Extreme", "Low Extreme")))

#Infectious Diseases
Diseases <- matrix(c(0.5, 1- 0.5),
                  byrow = TRUE, ncol = 2,
                  dimnames = list(NULL, c("High Diseases", "Low Diseases")))

#Water Turbidity
Turbidity <- matrix(c(0.814423077, 1- 0.814423077),
                  byrow = TRUE, ncol = 2,
                  dimnames = list(NULL, c("High Turbidity", "Low Turbidity")))

#Water Temperature
Temperature <- matrix(c(0.043333333, 0.405833333, 0.550833333),
                    byrow = TRUE, ncol = 3,
                    dimnames = list(NULL, c("High WT", "Medium WT", "Low WT")))

#Grazers (conditional on Water Temperature)
Grazers <- matrix(c(0.8075, 0.61, 0.3675, 0.1925, 0.39, 0.6325),
                  byrow = TRUE, ncol = 3,
                  dimnames = list(c("High Grazers","Low Grazers"), c("High WT", "Medium WT", "Low WT")))

#Grazers (Prior)
Grazers_Prior <- matrix(c(0.764814815, 1-0.764814815),
                  byrow = TRUE, ncol = 2,
                  dimnames = list(NULL, c("High Grazers","Low Grazers")))

#Alien Species (conditional on Water Temperature)
Alien_S <- matrix(c(0.439130435, 0.369565217, 0.3, 0.560869565, 0.630434783, 0.7),
                  byrow = TRUE, ncol = 3,
                  dimnames = list(c("High Species","Low Species"), c("High WT", "Medium WT", "Low WT")))

#Alien Species (Prior)
Alien_S_Prior <- matrix(c(0.606666667, 1-0.606666667),
                  byrow = TRUE, ncol = 2,
                  dimnames = list(NULL, c("High Species","Low Species")))


#######################################################
###Probability matrix of Kelp given each parent node###
#######################################################

#Bottom Trawling
Kelp.BT <- matrix(c(0.184210526, 0.273684211, 0.542105263, 0.515789474, 0.347368421, 0.136842105),
                  byrow = FALSE, ncol = 2,
                  dimnames = list(c("Good Kelp", "Average Kelp", "Low Kelp"), c("High BT","Low BT")))

#Burial Activities
Kelp.BA <- matrix(c(0.233333333, 0.49047619, 0.276190476, 0.461904762, 0.376190476, 0.161904762),
                  byrow = FALSE, ncol = 2,
                  dimnames = list(c("Good Kelp", "Average Kelp", "Low Kelp"), c("High Burial","Low Burial")))

#Extreme Events
Kelp.EE <- matrix(c(0.339130435, 0.369565217, 0.291304348, 0.565217391, 0.317391304, 0.117391304),
                  byrow = FALSE, ncol = 2,
                  dimnames = list(c("Good Kelp", "Average Kelp", "Low Kelp"), c("High Extreme","Low Extreme")))

#Alien Species
Kelp.AS <- matrix(c(0.3, 0.333333333, 0.366666667, 0.5, 0.4, 0.1),
                  byrow = FALSE, ncol = 2,
                  dimnames = list(c("Good Kelp", "Average Kelp", "Low Kelp"), c("High Species","Low Species")))

#Infectious Diseases
Kelp.ID <- matrix(c(0.283333333, 0.458333333, 0.258333333, 0.425, 0.416666667, 0.158333333),
                  byrow = FALSE, ncol = 2,
                  dimnames = list(c("Good Kelp", "Average Kelp", "Low Kelp"), c("High Diseases","Low Diseases")))

#Grazers
Kelp.GRAZER <- matrix(c(0.35952381, 0.283333333, 0.357142857, 0.471428571, 0.295238095, 0.233333333),
                  byrow = FALSE, ncol = 2,
                  dimnames = list(c("Good Kelp", "Average Kelp", "Low Kelp"), c("High Grazers","Low Grazers")))

#Water Turbidity
Kelp.TURB <- matrix(c(0.176190476, 0.357142857, 0.466666667, 0.561904762, 0.357142857, 0.080952381),
                      byrow = FALSE, ncol = 2,
                      dimnames = list(c("Good Kelp", "Average Kelp", "Low Kelp"), c("High Turbidity","Low Turbidity")))

#Water Temperature
Kelp.TEMP <- matrix(c(0.095652174, 0.286956522, 0.617391304, 
                      0.386956522, 0.32173913, 0.291304348, 
                      0.452173913, 0.304347826,0.243478261),
                    byrow = FALSE, ncol = 3,
                    dimnames = list(c("Good Kelp", "Average Kelp", "Low Kelp"), c("High WT", "Average WT","Low WT")))

#########################################################
###Probability matrix of Ecosystem Services given Kelp###
#########################################################

#Biodiversity
Biodiversity <- matrix(c(0.785714286 ,0.257142857 ,0.147619048 , 
                         0.133333333 ,0.266666667 , 0.257142857, 
                         0.080952381 ,0.476190476 ,0.595238095),
                byrow = FALSE, ncol = 3,
                dimnames = list(c("High Biodiversity", "Average Biodiversity", "Low Biodiversity"), c("Good Kelp", "Average Kelp", "Low Kelp")))

#Nursery Area
Nurs.Area <- matrix(c(0.68, 0.31, 0.14, 
                      0.24, 0.38, 0.27, 
                      0.08, 0.31, 0.59),
                       byrow = FALSE, ncol = 3,
                       dimnames = list(c("High Nursery", "Average Nursery", "Low Nursery"), c("Good Kelp", "Average Kelp", "Low Kelp")))

#Carbon Sequestration
C.Seq <- matrix(c(0.7725, 0.325, 0.1025, 
                  0.16, 0.415, 0.23, 
                  0.0675, 0.26, 0.6675),
                       byrow = FALSE, ncol = 3,
                       dimnames = list(c("High Sequestration", "Average Sequestration", "Low Sequestration"), c("Good Kelp", "Average Kelp", "Low Kelp")))

#Water Purification
Water.Pur <- matrix(c(0.77, 0.4, 0.11, 
                      0.15, 0.38, 0.225, 
                      0.08, 0.22, 0.665),
                       byrow = FALSE, ncol = 3,
                       dimnames = list(c("High Purification", "Average Purification", "Low Purification"), c("Good Kelp", "Average Kelp", "Low Kelp")))

#Aquaculture
Aquaculture <- matrix(c(0.519444444, 0.322222222, 0.125, 
                        0.480555556, 0.677777778, 0.875),
                       byrow = FALSE, ncol = 3,
                       dimnames = list(c("High Aquaculture", "Low Aquaculture"), c("Good Kelp", "Average Kelp", "Low Kelp")))

#Health Benefits
Health.Ben <- matrix(c(0.693333333, 0.406666667, 0.12, 
                       0.306666667, 0.593333333, 0.88),
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
capture.output(print(cpt_kelp), file = "kelp_cpt_viana.txt")

