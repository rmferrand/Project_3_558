#API.R 

suppressPackageStartupMessages({
#load in necessary libraries
library(plumber)
library(parsnip)
library(dplyr)
library(ggplot2)
library(yardstick)
library(tidymodels)
})

#use readRDS to quickly load in dataset and final fit of the dataset (see below...)
diabetes_final <- readRDS(file = "diabetes_final.rds")
rf_full_fit <- readRDS(file = "rf_full_fit.rds")


#here is the code necessary for running the workflow and fitting the final model. However, to save
#runtime, i have decided to just save the full fit of the model beforehand and read that in instead!

#rf_final_wkf <- readRDS(file = "rf_final_wkf.rds")
#rf_full_fit <- rf_final_wkf |> fit(diabetes_final)


#* Prediction Endpoint
#' This endpoint allows users to predict outcomes on the trained random forest by using queried predictors.
#' Default values are the most prevalent class. For example, most people eat fruits every day (1).
#' @param HighBP 0 = No, 1 = Yes (High Blood Pressure)
#' @param HighChol 0 = No, 1 = Yes (High Cholesterol)
#' @param Fruits 0 = No, 1 = Yes (Eats Fruit)
#' @param DiffWalk 0 = No, 1 = Yes (Difficulty Walking)
#' @param Smoker 0 = No, 1 = Yes (Smoked 100 Cigarettes)
#' @param CholCheck 0 = No, 1 = Yes (Had Regular Cholesterol Check)
#' @param HeartDiseaseorAttack 0 = No, 1 = Yes (Heart Attack/Disease)
#' @param HvyAlcoholConsump 0 = No, 1 = Yes (Large Alcohol Consumption)
#' @param GenHlth 1 = Excellent, 2 = Very Good, 3 = Good, 4 = Fair, 5 = Poor (General Health)
#' @param MentHlth 0 = No Pain 1 = Some Pain 2 = Everyday Pain (Mental Pain last 30 Days)
#' @param PhysHlth 0 = No Pain 1 = Some Pain 2 = Everyday Pain (Physical Pain last 30 Days)
#' @param Sex 0 = Female, 1 = Male (Gender)
#' @param Age 1 = 18-24, 2 = 25-29, ..., 12 = 75-79, 13 = 80+ (Category of Age)
#' @post /pred

function(HighBP = "0", 
         HighChol = "0",
         Fruits = "1",
         DiffWalk = "0",
         Smoker = "0",
         CholCheck = "1",
         HeartDiseaseorAttack = "0",
         HvyAlcoholConsump = "0",
         GenHlth = "2",
         MentHlth = "0", 
         PhysHlth = "0", 
         Sex = "0", 
         Age = "9") {
  #needed to create a "skeleton" dataset so that users could input predictions and make a fake one-row dataset
  default_diabetes <- data.frame(
    HighBP = factor(HighBP, levels = c(0, 1), labels = c("0", "1")),
    HighChol = factor(HighChol, levels = c(0, 1), labels = c("0", "1")),
    Fruits = factor(Fruits, levels = c(0, 1), labels = c("0", "1")),
    DiffWalk = factor(DiffWalk, levels = c(0, 1), labels = c("0", "1")),
    Smoker = factor(Smoker, levels = c(0, 1), labels = c("0", "1")),
    CholCheck = factor(CholCheck, levels = c(0, 1), labels = c("0", "1")),
    HeartDiseaseorAttack = factor(HeartDiseaseorAttack, levels = c(0, 1), labels = c("0", "1")),
    HvyAlcoholConsump = factor(HvyAlcoholConsump, levels = c(0, 1), labels = c("0", "1")),
    GenHlth = factor(GenHlth, levels = c(1:5), labels = c("Excellent", "Very Good", "Good", "Fair", "Poor"),ordered = TRUE),
    MentHlth = factor(MentHlth, levels = c(0:2), labels = c("No Pain", "Some Pain", "Everyday Pain"), ordered = TRUE),
    PhysHlth = factor(PhysHlth, levels = c(0:2), labels = c("No Pain", "Some Pain", "Everyday Pain"), ordered = TRUE),
    Sex = factor(Sex, levels = c(0,1), labels = c("Female", "Male")),
    Age = factor(Age, levels = c(1:13), labels = c("18-24", "25-29", "30-34", "35-39", "40-44", "45-49", 
                                                   "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", 
                                                   "80-99"), ordered = TRUE)
    )
  
  prediction <- predict(rf_full_fit, default_diabetes, type = "class")
  
  list(prediction = as.character(prediction$.pred_class))
}


#example test calls. the port stuff is really weird, Let's do port 8000, or replace 8000 with desired port.
#r<- plumb("API.R")
#r$run(port=8000)

# curl -X POST -H "Content-Type: application/json" -d '{}' http://localhost:8000/pred
# curl -X POST -H "Content-Type: application/json" -d '{"GenHlth": "5", "HighBP": "1", "HighChol": "1", "Fruits": "1"}' http://localhost:8000/pred
# curl -X POST -H "Content-Type: application/json" -d '{"HighBP": "0", "HighChol": "0", "Fruits": "0", "DiffWalk": "0", "Smoker": "1", "CholCheck": "1", "HeartDiseaseorAttack": "0", "HvyAlcoholConsump": "0", "GenHlth": "3", "MentHlth": "0", "PhysHlth": "1", "Sex": "0", "Age": "1"}' http://localhost:8000/pred


#* Info Endpoint
#' Provides information about the API creator and provides a URL for the data exploration.
#' @get /info

function() {
"This API was created by Robert (Robbie) Ferrand.  Check out Project 3 558 here: https://rmferrand.github.io/Project_3_558/EDA.html Thanks Justin!"
}

#* Confusion Endpoint
#' Allows the user to obtain a confusion matrix of the fully fitted data. This will output an image.png.
#* @get /confusion
#* @serializer png

#create the function that calculates confusion matrix for full data
function() {
  prediction <- predict(rf_full_fit, diabetes_final, type = "class")
  
  results <- diabetes_final |>
    mutate(Predicted = prediction$.pred_class)
  
  confusion <- conf_mat(data = results, truth = Diabetes_binary, estimate = Predicted)
  confusion_plot <- autoplot(confusion, type = "heatmap") +
    scale_fill_gradient(low = "pink", high = "red") +
    ggtitle("Confusion Matrix") +
    xlab("Predicted") +
    ylab("Actual") +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 14),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
      axis.text.y = element_text(size = 12)
    )
  print(confusion_plot)
}
