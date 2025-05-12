rm(list = ls()) #Clearing the environment

# Load packages (improved)
library(bnlearn)
library(readxl)

#Setting up data --------------------------------------------------------------
#Calling Data (Requires that the excel file 
#be located in the same folder as the code)
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
getwd()
kelp_data <- read_excel("DATASET.xlsx", sheet = "Design_1")
dfdata <- data.frame(kelp_data)
bn_data <- dfdata[, -1] #Removes the first column (ID)
View(bn_data)

#Discretizing the data ---------------------------------------------------
kelp_ind <- data.frame(bn_data[, 1]) #kelp_ind - kelp indicator
grazers <- data.frame(bn_data[, 2]) #grazers - grazers indicator
mean_fetch <- data.frame(bn_data[, 3]) #mean fetch
turb <- data.frame(bn_data[, 5]) #turb - turbidity
temp <- data.frame(bn_data[, 6]) #temp - temperature
upwell <- data.frame(bn_data[, 8]) #upwell - upwelling

#Create a new environment to store discretized variables
discretized_env <- new.env()

#List of variables to discretize and remove from the original environment
vars_to_remove <- c("kelp_ind", "grazers", "mean_fetch", "turb", "temp",
                    "upwell")

#Loop through vars to discretize and remove from original environment
for (var_name in vars_to_remove) {
  #Get the var from the original environment
  var <- get(var_name, envir = .GlobalEnv)
  
  #Discretize the variable between 2 and 5 intervals
  discretized <- list()
  for (i in 2:5) {
    disc_var <- discretize(var,
                           method = "quantile",
                           breaks = i,  
                           ordered = FALSE,
                           debug = TRUE)
    var_name_disc <- paste(var_name, "_", i, sep = "")
    discretized[[var_name_disc]] <- disc_var
    assign(var_name_disc, disc_var, envir = discretized_env)
  }
  
  # Remove the variable from the original environment
  rm(list = var_name, envir = .GlobalEnv)
}

###Renaming the columns:
#Get the names of variables in the discretized_env
discretized_variables <- ls(envir = discretized_env)
discretized_variables

#Define the renaming rules
renaming_rules <- list(
  "kelp_ind" = "Kelp_Ind",
  "grazers" = "Grazers",
  "mean_fetch" = "Mean_Fetch",
  "turb" = "Turbidity",
  "temp" = "Temperature",
  "upwell" = "Upwelling"
)

#Loop through variables and apply renaming rules
for (prefix in names(renaming_rules)) {
  #Find variables starting with the specified prefix
  pattern <- paste0("^", prefix) #regex for beginning of string
  matching_vars <- grep(pattern,
                        discretized_variables,
                        value = TRUE)

  #Rename columns for matching variables
  for (var_name in matching_vars) {
    #Get the variable from the environment
    variable_to_rename <- get(var_name, envir = discretized_env)

    #Rename the specified column
    colnames(variable_to_rename)[1] <- renaming_rules[[prefix]]

    #Assign the modified variable back to the environment
    assign(var_name, variable_to_rename, envir = discretized_env)
  }
}

#Organizing the dataframes ----------------------------------------------------
#Create a new environment to store combined dataframes
final_dataframes <- new.env()

#Define the variables to combine
variables_to_combine <- c("kelp_ind", "grazers", "mean_fetch", "turb", "temp",
                          "upwell")

#Get all possible combinations of intervals
intervals <- 2:5

#Generate all combinations of intervals for each variable
interval_combinations <- expand.grid(replicate(length(variables_to_combine),
                                            intervals, simplify = FALSE))

#Loop through each combination of intervals
for (i in seq_len(nrow(interval_combinations))) {
  #Create a list to hold the dataframes for this combination of intervals
  combined_dfs <- list()

  #Loop through the variables to combine and create the dataframe
  for (var_idx in seq_along(variables_to_combine)) {
    #Extract the dataframe for the specific interval
    df <- get(paste0(variables_to_combine[var_idx],
                     "_",
                     interval_combinations[i, var_idx]),
              envir = discretized_env)

    #Add the dataframe to the list
    combined_dfs[[variables_to_combine[var_idx]]] <- df
  }

  #Combine the dataframes horizontally
  combined_df <- do.call(cbind, combined_dfs)

  #Store the combined dataframe in final_dataframes environment
  df_name_combined <- paste(interval_combinations[i, ], collapse = "")
  assign(df_name_combined, combined_df, envir = final_dataframes)
}

#Display the final combined dataframes
View(final_dataframes)

#Exporting Dataframes ----------------------------------------------------
#Save the Dataframes environment to a file
save(final_dataframes, file = "dataframes_env.RData")



