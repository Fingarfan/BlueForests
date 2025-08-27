rm(list = ls()) #Clearing the environment

library(dplyr)
library(readxl)
library(bnlearn)

set.seed(42)

#Calling Data
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
kelp_data <- read_excel("DATASET.xlsx", sheet = "Design_1")
View(kelp_data)
dfdata <- data.frame(kelp_data)
BN_data <- dfdata[,-1]

#Discretizing the data ---------------------------------------------------
K_IND <- data.frame(BN_data[,1]) #kelp_ind - kelp indicator
GRAZERS <- data.frame(BN_data[,2]) #grazers - grazers indicator
M_FETCH <- data.frame(BN_data[,3]) #mean fetch
TURB <- data.frame(BN_data[,5]) #turb - turbidity
TEMP <- data.frame(BN_data[,6]) #temp - temperature
UPWELL <- data.frame(BN_data[,8]) #upwell - upwelling

BN_data2 <- data.frame(K_IND, GRAZERS, M_FETCH, TURB, TEMP, UPWELL)

#Resampling and dataset simulation ---------------------------------------------
library(dplyr)

#Defining the variables to resample and the number of replications
n_variables <- ncol(BN_data2)
n_replications <- 100

#Resampling for each variable 
simul_datasets <- vector("list", length = n_variables)


for (variable_index in 1:n_variables) {
  original_var <- BN_data2[, variable_index]
  simul_var <- vector("list", n_replications)
  
  for (i in 1:n_replications) {
    simul_obs <- sample(original_var)  #Random sampling to get each new observation without replacement
    simul_var[[i]] <- simul_obs
  }
  
  simul_datasets[[variable_index]] <- simul_var
}

#Combining the datasets
combined_simul_datasets <- lapply(simul_datasets, function(simul_var) {
  do.call(rbind, simul_var)
})


# Define variable names
variable_names <- c("Kelp Indicator", "Grazers", "Mean Fetch", "Turbidity", "Temperature", "Upwelling")

# Kolmogorov-Smirnov test, Q-Q plots for each variable
(mfrow = c(1, 1))

for (variable_index in 1:n_variables) {
  original_var <- BN_data2[, variable_index]
  combined_new_var <- combined_simul_datasets[[variable_index]]
  ks_test_results[[variable_index]] <- ks.test(original_var, combined_new_var)
  qq_plots[[variable_index]] <- qqplot(original_var, combined_new_var, 
                                       xlab = paste("Original", variable_names[variable_index]), 
                                       ylab = paste("Simulated", variable_names[variable_index]),
                                       main = paste("Q-Q Plot:", variable_names[variable_index]),
                                       pch = 21,           
                                       cex = 0.7,          
                                       col = "navy",       
                                       bg = alpha("steelblue", 0.6),  
                                       font.main = 2,      
                                       font.lab = 2,       
                                       cex.main = 1.2,     
                                       cex.lab = 1.1,      
                                       cex.axis = 0.9,     
                                       las = 1)            
  
  abline(a = 0, b = 1, col = "red3", lwd = 2.5, lty = 2)
  grid(col = "lightgray", lty = 3, lwd = 0.5)
  points(qq_plots[[variable_index]]$x, qq_plots[[variable_index]]$y,
         pch = 21, cex = 0.7, col = "navy", bg = alpha("steelblue", 0.6))
  abline(a = 0, b = 1, col = "red3", lwd = 2.5, lty = 2)
}

#DATA REARRANGING ---------------------------------------------------------
#Create the 'simulation' environment
Simul_Data <- new.env()

#Loop through each variable's combined resampled data
for (var_index in seq_along(combined_simul_datasets)) {
  #Create a list to store data frames for each resample of this variable
  arranged_dataframes <- list()
  
  #Extract the combined resampled data for this variable
  combined_var_simul <- combined_simul_datasets[[var_index]]
  
  #Loop through each row (resample) in the combined resampled data
  for (simul_index in seq_len(nrow(combined_var_simul))) {
    #Create a data frame for each resample and store it in a list
    resampled_df <- as.data.frame(combined_var_simul[simul_index, , drop = FALSE])
    arranged_dataframes[[paste0("Resample_", simul_index)]] <- resampled_df
  }
  
  #Store the list of data frames for this variable in the 'simulations' environment
  assign(paste0("Variable_", var_index), arranged_dataframes, envir = Simul_Data)
}

#Remove the original objects from the global environment
rm(combined_simul_datasets)


#Define the list of variables and their corresponding names
vars_to_select <- c(1, 2, 3, 4, 5, 6)
var_names <- c("K_IND", "GRAZERS", "M_FETCH", "TURB", "TEMP", "UPWELL")

#Loop through each variable
for (i in seq_along(vars_to_select)) {
  #Extract the data frames for the current variable and its simulations
  arranged_dataframes <- Simul_Data[[paste0("Variable_", vars_to_select[i])]]
  
  #Loop through each simulation
  for (simul_index in seq_along(arranged_dataframes)) {
    #Get the data frame for the current simulation
    simul_df <- arranged_dataframes[[simul_index]]
    
    #Rename the variable within the data frame
    colnames(simul_df) <- var_names[i]
    
    #Assign the renamed data frame to the simulations environment with appropriate indexing
    assign(paste0(var_names[i], "_P", simul_index), simul_df, envir = Simul_Data)
  }
}


#Dataframe Organization Part #1 ----------------------------------------
#Create a new environment to store rearranged dataframes
rearranged_Simul_Data <- new.env()

#Loop through each simulation index
for (n in 1:n_replications) {
  #Create a dataframe for the current simulation index
  combined_df <- data.frame(
    GRAZERS_P = as.vector(t(get(paste("GRAZERS_P", n, sep = ""), envir = Simul_Data))),
    K_IND_P = as.vector(t(get(paste("K_IND_P", n, sep = ""), envir = Simul_Data))),
    M_FETCH_P = as.vector(t(get(paste("M_FETCH_P", n, sep = ""), envir = Simul_Data))),
    TEMP_P = as.vector(t(get(paste("TEMP_P", n, sep = ""), envir = Simul_Data))),
    TURB_P = as.vector(t(get(paste("TURB_P", n, sep = ""), envir = Simul_Data))),
    UPWELL_P = as.vector(t(get(paste("UPWELL_P", n, sep = ""), envir = Simul_Data)))
  )
  
  #Update variable names based on the dataframe number
  var_names <- colnames(combined_df)
  for (var_index in seq_along(var_names)) {
    new_var_name <- paste0(var_names[var_index], n)
    colnames(combined_df)[var_index] <- new_var_name
  }
  
  #Store the combined dataframe in the rearranged_Boot_Data environment
  df_name <- paste("Dataframe", n, sep = "_")
  assign(df_name, combined_df, envir = rearranged_Simul_Data)
}

#Check the first few rows of the rearranged dataframes
head(rearranged_Simul_Data$Dataframe_1)
head(rearranged_Simul_Data$Dataframe_2)
#Repeat for other dataframes should you want to see them

#Save the rearranged dataframes to a file if needed
save(rearranged_Simul_Data, file = "rearranged_Boot_Data.RData")

#Discretization ------------------------------------------------
#Create a new environment to store discretized variables
disc_Simul_Data <- new.env()

#Number of intervals
intervals <- 2:5

#Loop through each dataframe index
for (n in 1:n_replications) {  #Looping over the number of replications
  #Get the original dataframe
  original_df <- rearranged_Simul_Data[[paste("Dataframe", n, sep = "_")]]
  
  #Create a list to store discretized variables for each variable
  discretized_variables <- list()
  
  #Loop through each variable
  for (var_name in colnames(original_df)) {
    #Loop through each number of intervals
    for (num_intervals in intervals) {
      #Extract the variable from the original dataframe
      original_var <- data.frame(original_df[[var_name]])
      colnames(original_var) <- var_name
      
      #Discretize the variable with the current number of intervals using discretize
      discretized_var <- discretize(original_var, method = "quantile", breaks = num_intervals)
      
      #Renaming properly the columns
      #Extract the variable name without the P and following numbers
      base_var_name <- sub("_P\\d+$", "", var_name)
      
      #Define the column mapping
      column_mapping <- c("K_IND" = "Kelp_Ind",
                          "GRAZERS" = "Grazers",
                          "M_FETCH" = "Mean_Fetch",
                          "TURB" = "Turbidity",
                          "TEMP" = "Temperature",
                          "UPWELL" = "Upwelling")
      
      #Get the original column name
      original_col_name <- base_var_name
      
      #Map the original column name to the new name
      new_col_name <- column_mapping[original_col_name]
      #Update the column names of the discretized variable
      colnames(discretized_var) <- new_col_name
      
      #Create a name for the new variable
      new_var_name <- paste0(var_name, "_", num_intervals)
      
      #Store the discretized variable in the list
      discretized_variables[[new_var_name]] <- discretized_var
    }
  }
  
  #Store the list of discretized variables for the current dataframe in the environment
  assign(paste("Simulation", n, sep = "_"), discretized_variables, envir = disc_Simul_Data)
}





#Dataframe Organization Part#2----------------------------------------
#Create a new environment to store combined dataframes for resampling
Simul_F_DATA <- new.env()

#Define the variables to combine
variables_to_combine <- c("K_IND", "GRAZERS", "M_FETCH", "TURB", "TEMP", "UPWELL")

#Get all possible combinations of intervals
intervals <- 2:5

#Loop through each simulation index
for (simulation_index in 1:100) {
  #Get the list of dataframes for the current simulation index
  simulation_list <- disc_Simul_Data[[paste("Simulation", simulation_index, sep = "_")]]
  
  #Generate all combinations of intervals for each variable
  interval_combinations <- expand.grid(replicate(length(variables_to_combine), intervals, simplify = FALSE))
  
  #Create a list to store combined dataframes for the current simulation
  combined_simulation <- list()
  
  #Loop through each combination of intervals
  for (i in seq_len(nrow(interval_combinations))) {
    #Create a list to hold the dataframes for this combination of intervals
    combined_dfs <- list()
    
    #Loop through the variables to combine and create the dataframe
    for (var_idx in seq_along(variables_to_combine)) {
      #Extract the dataframe for the specific interval from the simulation list
      df <- simulation_list[[paste0(variables_to_combine[var_idx], "_P", simulation_index, "_", interval_combinations[i, var_idx])]]
      
      #Add the dataframe to the list
      combined_dfs[[variables_to_combine[var_idx]]] <- df
    }
    
    #Combine the dataframes horizontally
    combined_df <- do.call(cbind, combined_dfs)
    
    #Store the combined dataframe in the list for the current simulation
    df_name_combined <- paste(interval_combinations[i, ], collapse = "")
    combined_simulation[[df_name_combined]] <- combined_df
  }
  
  #Store the list of combined dataframes for the current simulation in the environment
  assign(paste("Simulation", simulation_index, sep = "_"), combined_simulation, envir = Simul_F_DATA)
}


#Exporting Dataframes ----------------------------------------------------
save(Simul_F_DATA, file = "Simul_F_DATA.RData")

