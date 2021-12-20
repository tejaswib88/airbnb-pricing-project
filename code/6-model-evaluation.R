# load libraries
library(glmnetUtils)
library(tidyverse)

#Load testing data
listings = read.csv("data/clean/cleaned_listings.csv")
listings = listings %>% mutate_if(is.character,as.factor)

set.seed(1)
#Generate training samples & split
train_samples = sample(1:nrow(listings), 0.8*nrow(listings))
listings_train = listings %>% filter(row_number() %in% train_samples)
listings_test = listings %>% filter(!(row_number() %in% train_samples))

#Load saved models
load("results/ridge_fit.Rda")
load("results/lasso_fit.Rda")
load("results/rf_finalfit.Rda")
load("results/linear_fit.Rda")

#Create predictions for linear model
linear_predictions = predict(lm_fit,
                             newdata = listings_test) %>% as.numeric()

#Calculate RMSE for linear model
linear_rmse = sqrt(mean((linear_predictions-listings_test$price)^2))

#Create predictions for ridge model
ridge_predictions = predict(ridge_fit, 
                            newdata = listings_test,
                            s = "lambda.1se") %>% as.numeric()
#Calculate RMSE for ridge model
ridge_rmse = sqrt(mean((ridge_predictions - listings_test$price)^2))

#Create predictions for lasso model
lasso_predictions = predict(lasso_fit, 
                            newdata = listings_test,
                            s = "lambda.1se") %>% as.numeric()
#Calculate RMSE for lasso model
lasso_rmse = sqrt(mean((lasso_predictions - listings_test$price)^2))

#Create predictions for benchmark intercept model
intercept_predictions = rep(mean(listings_train$price), nrow(listings_test))
intercept_rmse = sqrt(mean((intercept_predictions - listings_test$price)^2))

#Create predictions for random forest model
rf_predictions = predict(rf_finalfit, newdata = listings_test)

#Calculate RMSE for random forest model
rf_rmse = sqrt(mean((rf_predictions - listings_test$price)^2))

#Create tibble comparing RMSEs accross model and save as a CSV
p = tibble(Model = c("Intercept Only", "Linear Model", "Ridge", "Lasso", "Random Forest"),
       RMSE = c(intercept_rmse, linear_rmse, ridge_rmse, lasso_rmse, rf_rmse))

write_csv(x = p, file = "results/model-evaluation.csv")
