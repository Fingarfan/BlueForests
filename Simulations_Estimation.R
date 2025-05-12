rm(list = ls()) #Clearing the environment
gc()

library(bnlearn)

#Set seed for reproducibility
set.seed(42)

#Create environments at the beginning
BN_OUTPUT_Simul <- new.env()
CV_Results <- new.env()
CV_Results_Loss <- new.env()

#Setting up data --------------------------------------------------------------
#Calling Data (Requires that the dataframes environment 
#be located in the same folder as the code)
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
load("Simul_F_DATA.RData")

#BN Estimation -----------------------------------------------------------
dag_kelp <- empty.graph(nodes = c("Kelp_Ind",
                                  "Grazers", 
                                  "Mean_Fetch",  
                                  "Turbidity", 
                                  "Temperature",
                                  "Upwelling"))

arc.set_kelp <- matrix(c(
  "Grazers", "Kelp_Ind",
  "Temperature", "Grazers",
  "Temperature", "Kelp_Ind",
  "Mean_Fetch", "Kelp_Ind",
  "Upwelling",  "Kelp_Ind",
  "Turbidity", "Kelp_Ind"),
  byrow = TRUE, ncol = 2,
  dimnames = list(NULL, c("from", "to")))

arcs(dag_kelp) <- arc.set_kelp
nodes(dag_kelp)
arcs(dag_kelp)
plot(dag_kelp)
dag_kelp

g1 <- graphviz.plot(dag_kelp)
graph::nodeRenderInfo(g1) <- list(fontsize=30, shape = "ellipse")
Rgraphviz::renderGraph(g1)

nodes(dag_kelp)

#Function to fit Bayesian networks
fit_bayesian_networks <- function(data_env, dag, methods = c("mle", "bayes")) {
  output_env <- new.env()
  simulation_names <- ls(envir = data_env)
  
  total_sims <- length(simulation_names)
  cat("Processing", total_sims, "simulations for fitting\n")
  
  for (sim_idx in 1:total_sims) {
    sim_name <- simulation_names[sim_idx]
    cat("Processing simulation", sim_idx, "of", total_sims, ":", sim_name, "\n")
    
    tryCatch({
      sim_list <- get(sim_name, envir = data_env)
      output_simulation <- list()
      
      for (df_name in names(sim_list)) {
        df <- sim_list[[df_name]]
        
        for (method in methods) {
          tryCatch({
            fit <- bn.fit(dag, df, method = method, replace.unidentifiable = TRUE)
            output_name <- paste(df_name, "_fit_", method, sep = "")
            output_simulation[[output_name]] <- fit
          }, error = function(e) {
            cat("Error fitting", method, "for", df_name, "in", sim_name, ":", conditionMessage(e), "\n")
          })
        }
      }
      
      assign(sim_name, output_simulation, envir = output_env)
    }, error = function(e) {
      cat("Error processing simulation", sim_name, ":", conditionMessage(e), "\n")
    })
  }
  
  return(output_env)
}

perform_cross_validation <- function(models_env, data_env, dag, k = 10) {
  cv_results_env <- new.env()
  cv_loss_env <- new.env()
  simulation_min_loss <- list()
  
  simulation_names <- ls(envir = models_env)
  total_sims <- length(simulation_names)
  cat("Processing", total_sims, "simulations for cross-validation\n")
  
  for (sim_idx in 1:total_sims) {
    sim_name <- simulation_names[sim_idx]
    cat("Processing simulation", sim_idx, "of", total_sims, ":", sim_name, "\n")
    
    tryCatch({
      sim_models <- get(sim_name, envir = models_env)
      all_model_losses <- c()
      
      for (model_name in names(sim_models)) {
        bn_model <- sim_models[[model_name]]
        
        #Extract the model number from the model name
        model_number <- substr(model_name, 1, 6)
        
        #Form the dataset name based on the model number
        dataset_name <- paste(model_number, sep = "")
        
        #Extract the dataset from data_env
        dataset <- data_env[[sim_name]][[dataset_name]]
        
        #Skip if dataset is NULL or has issues
        if (is.null(dataset) || nrow(dataset) == 0) {
          cat("Warning: Invalid dataset for", model_name, "in", sim_name, "\n")
          next
        }
        
        tryCatch({
          #Perform cross-validation with explicit 10 folds
          cv_result <- bn.cv(data = dataset, bn = dag, loss = 'logl', method = "k-fold", k = 10)

          fold_losses <- c()
          for (i in 1:10) {
            if (i <= length(cv_result) && !is.null(cv_result[[i]][["loss"]])) {
              fold_losses[i] <- cv_result[[i]][["loss"]]
            } else {
              fold_losses[i] <- NA
            }
          }
          
          #Calculate mean with NA removal
          loss <- mean(fold_losses, na.rm = TRUE)
          
          #Check if loss is valid
          if (is.na(loss) || is.nan(loss) || is.infinite(loss)) {
            loss <- 999999
          }
          
          #Store the cross-validation result
          result_name <- paste(model_name, "_cv_result", sep = "")
          loss_values <- paste(model_name, "_loss", sep = "")
          assign(result_name, cv_result, envir = cv_results_env)
          assign(loss_values, loss, envir = cv_loss_env)
          
          #Store the loss for this model
          all_model_losses[model_name] <- loss
          
        }, error = function(e) {
          cat("Error in CV for", model_name, "in", sim_name, ":", conditionMessage(e), "\n")
          #Assign a large loss value for failed models
          all_model_losses[model_name] <- 999999
        })
      }
      
      if (length(all_model_losses) > 0) {
        #Find the model with the minimum loss
        min_name <- names(which.min(all_model_losses))
        min_value <- all_model_losses[min_name]
        
        #Check if min_value is our error placeholder
        if (min_value == 999999) {
          min_value <- NA
        }
        
        #Store the minimum loss for the current simulation
        simulation_min_loss[[sim_name]] <- list(min_name = min_name, min_value = min_value)
        
        #Print the results for this simulation
        cat("  Best model:", min_name, "\n")
        cat("  Minimum loss value:", min_value, "\n\n")
      } else {
        cat("No valid models found for simulation", sim_name, "\n\n")
      }
    }, error = function(e) {
      cat("Error processing simulation", sim_name, ":", conditionMessage(e), "\n\n")
    })
  }
  
  #Create a summary dataframe
  summary_results <- do.call(rbind, lapply(names(simulation_min_loss), function(sim) {
    data.frame(
      Simulation = sim,
      Best_Model = simulation_min_loss[[sim]]$min_name,
      Min_Loss = simulation_min_loss[[sim]]$min_value,
      stringsAsFactors = FALSE
    )
  }))
  
  #Assign the summary to cv_loss_env
  assign("summary_results", summary_results, envir = cv_loss_env)
  assign("simulation_min_loss", simulation_min_loss, envir = cv_loss_env)
  
  return(list(cv_results = cv_results_env, cv_loss = cv_loss_env))
}


#Execution Block
#Fit the Bayesian networks
cat("Starting Bayesian network fitting process...\n")
BN_OUTPUT_Simul <- fit_bayesian_networks(Simul_F_DATA, dag_kelp, methods = c("mle", "bayes"))
save(BN_OUTPUT_Simul, file = "bn_output_simul_env.RData")
cat("Bayesian network fitting completed and saved.\n\n")

#Perform cross-validation
cat("Starting cross-validation process...\n")
cv_output <- perform_cross_validation(BN_OUTPUT_Simul, Simul_F_DATA, dag_kelp, k = 10)
CV_Results <- cv_output$cv_results
CV_Results_Loss <- cv_output$cv_loss

#Get the summary results
summary_results <- get("summary_results", envir = CV_Results_Loss)

#Print the results
cat("Summary of Cross-Validation Results:\n")
for (i in 1:nrow(summary_results)) {
  cat(sprintf("Simulation %s: Best performing model is %s with %.4f mean loss\n", 
              summary_results$Simulation[i],
              summary_results$Best_Model[i],
              summary_results$Min_Loss[i]))
}

#Save the results
save(CV_Results, CV_Results_Loss, summary_results, file = "cv_results_summary.RData")
cat("\nCross-validation completed and results saved.\n")