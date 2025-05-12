rm(list = ls())

library(bnlearn)

#Data Loading
load("dataframes_env.RData")
set.seed(42)

#DAG Setup
dag_kelp <- empty.graph(nodes = c("Kelp_Ind", "Grazers", "Mean_Fetch", "Turbidity", "Temperature", "Upwelling"))

set_kelp_arcs <- matrix(c("Grazers", "Kelp_Ind",
                          "Temperature", "Grazers",
                          "Temperature", "Kelp_Ind",
                          "Mean_Fetch", "Kelp_Ind",
                          "Upwelling", "Kelp_Ind",
                          "Turbidity", "Kelp_Ind"),
                        byrow = TRUE,
                        ncol = 2,
                        dimnames = list(NULL, c("from", "to")))

arcs(dag_kelp) <- set_kelp_arcs
plot(dag_kelp)
graphviz.plot(dag_kelp) 


#Model Fitting and Cross-Validation
bn_models <- list()
cv_results <- list()

for (df_name in ls(envir = final_dataframes)) { 
  df <- get(df_name, envir = final_dataframes) 
  
  #MLE Fitting
  fit_mle <- bn.fit(dag_kelp, df, method = "hard-em", replace.unidentifiable = TRUE)
  bn_models[[paste0(df_name, "_mle")]] <- fit_mle
  
  #Bayes Fitting
  fit_bayes <- bn.fit(dag_kelp, df, method = "bayes", replace.unidentifiable = TRUE)
  bn_models[[paste0(df_name, "_bayes")]] <- fit_bayes
  
  #Cross-Validation
  cv_result <- bn.cv(data = df, bn = dag_kelp, loss = "logl", method = "k-fold", k = 10)
  
  #Loss Calculation
  losses <- sapply(cv_result, function(x) x$loss)
  avg_loss <- mean(losses)
  
  cv_results[[paste0(df_name, "_mle_cv")]] <- list(cv_result = cv_result, avg_loss = avg_loss, model_type = "mle")
  cv_results[[paste0(df_name, "_bayes_cv")]] <- list(cv_result = cv_result, avg_loss = avg_loss, model_type = "bayes")
}

save(bn_models, file = "bn_models.RData")
save(cv_results, file = "cv_results.RData")


#Minimum Loss Model
min_loss <- Inf
min_model_name <- NULL

for (cv_name in names(cv_results)) {
  avg_loss <- cv_results[[cv_name]]$avg_loss
  if (avg_loss < min_loss) {
    min_loss <- avg_loss
    min_model_name <- cv_name
  }
}

cat("The model with lowest loss value is:", min_model_name, "\n")
cat("Minimum value:", min_loss, "\n")


#Accessing the best model and its CV results
best_model_name <- min_model_name
best_model_cv <- cv_results[[best_model_name]]
best_model <- bn_models[[gsub("_cv", "", best_model_name)]] 

print(best_model_cv)
print(best_model)
