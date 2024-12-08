#myAPI.R 

library(plumber)

# Load required libraries
library(parsnip)
library(dplyr)

rf_full_fit <- rf_final_wkf |>
  fit(diabetes_final)

final_rf_model <- extract_fit_parsnip(rf_full_fit)
# Get the predictors used
print(final_rf_model$fit$forest$independent.variable.names)



#* Prediction Endpoint
#' Predict outcomes using the trained model.
#' Accepts predictors as query parameters, with default values.
#' @param GenHlth General health rating
#' @param MentHlth Mental health condition
#' @param PhysHlth Physical health condition
#' @param Sex Gender
#' @param Age Age
#' @param Education Highest level of education
#' @param Income Income bracket
#' @post /pred

function(AnyHealthcare = "1", 
         NoDocbcCost = "0",
         GenHlth = "Very Good", 
         MentHlth = "No Pain", 
         PhysHlth = "No Pain", 
         Sex = "Female", 
         Age = "60-64",
         Education = "College graduate", 
         Income = ">=$75,000") {
  
  default_diabetes <- data.frame(
    AnyHealthcare = factor(AnyHealthcare, levels = c(0, 1), labels = c("0", "1")),
    NoDocbcCost = factor(NoDocbcCost, levels = c(0, 1), labels = c("0", "1")),
    GenHlth = factor(GenHlth, levels = c(1:5), labels = c("Excellent", "Very Good", "Good", "Fair", "Poor"),ordered = TRUE),
    MentHlth = factor(MentHlth, levels = c(1:3), labels = c("No Pain", "Some Pain", "Everyday Pain"), ordered = TRUE),
    PhysHlth = factor(PhysHlth, levels = c(1:3), labels = c("No Pain", "Pain", "Everyday Pain"), ordered = TRUE),
    Sex = factor(Sex, levels = c(1:2), labels = c("Female", "Male")),
    Age = factor(Age, levels = c(1:13), labels = c("18-24", "25-29", "30-34", "35-39", "40-44", "45-49", 
                                                   "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", 
                                                   "80-99"), ordered = TRUE),
    Education = factor(Education, levels = c(1:6), labels = c("NoSchool", "Elementary", "Some high school", 
                                                              "High school graduate", 
                                                              "Some college or technical school", 
                                                              "College graduate"), ordered = TRUE),
    Income = factor(Income, levels = c(1:8), labels = c("<$10,000",
                                                        "$10,000-$14,999",
                                                        "$15,000-$19,999",
                                                        "$20,000-$24,999",
                                                        "$25,000 - $34,999",
                                                        "$35,000 - $49,999",
                                                        "$50,000 - $74,999",
                                                        ">=$75,000"), ordered = TRUE)
    )
  
  prediction <- predict(rf_full_fit, default_diabetes, type = "class")
  
  list(prediction = as.character(prediction$.pred_class))
}

# Example API test calls
# curl -X POST -H "Content-Type: application/json" -d '{"GenHlth": "Very Good", "MentHlth": "No Pain", "PhysHlth": "No Pain", "Sex": "Female", "Education": "College graduate", "Income": ">= $75,000"}' http://localhost:8000/pred
# curl -X POST -H "Content-Type: application/json" -d '{}' http://localhost:8000/pred
# curl -X POST -H "Content-Type: application/json" -d '{"GenHlth": "Excellent"}' http://localhost:8000/pred