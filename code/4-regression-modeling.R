# load libraries
library(glmnetUtils)                    # to run ridge and lasso
source("code/functions/plot_glmnet.R")            # for lasso/ridge trace plots

#Read the training data
listings = read.csv("data/clean/cleaned_listings.csv")
listings = listings %>% mutate_if(is.character,as.factor)

set.seed(1)
#Generate training samples & split
train_samples = sample(1:nrow(listings), 0.8*nrow(listings))
listings_train = listings %>% filter(row_number() %in% train_samples)

#Fit and save linear regression model
set.seed(1)
lm_fit = lm(price ~ ., data = listings_train)
save(lm_fit, file = "results/linear_fit.Rda")

#Save summary
summary = as_tibble(summary(lm_fit)$coefficients)
write_csv(x = summary, file = "results/linear-model-summary.csv")

#Fit and save ridge model
set.seed(1)
ridge_fit = cv.glmnet(price ~ .,  # formula notation, as usual
                      alpha = 0,                 # alpha = 0 for ridge
                      nfolds = 10,               # number of folds
                      data = listings_train)
save(ridge_fit, file = "results/ridge_fit.Rda")


#Create and save lambda plot
p = plot(ridge_fit)

ggsave(filename = "results/ridge-lambda-plot.png", 
       plot = p, 
       device = "png", 
       width = 6, 
       height = 4)

#Create and save plot identifying first 10 features
p = plot_glmnet(ridge_fit, listings_train, features_to_plot = 10)

ggsave(filename = "results/ridge-features-plot.png", 
       plot = p, 
       device = "png", 
       width = 6, 
       height = 4)

#Fit and save lasso model
set.seed(1)
lasso_fit = cv.glmnet(price ~ .,  # formula notation, as usual
                      alpha = 1,                 # alpha = 0 for ridge
                      nfolds = 10,               # number of folds
                      data = listings_train)
save(lasso_fit, file = "results/lasso_fit.Rda")

#Create and save lambda plot
p = plot(lasso_fit)

ggsave(filename = "results/lasso-lambda-plot.png", 
       plot = p, 
       device = "png", 
       width = 6, 
       height = 4)

#Plot 10 features
p = plot_glmnet(lasso_fit, listings_train, features_to_plot = 10)

ggsave(filename = "results/lasso-features-plot.png", 
       plot = p, 
       device = "png", 
       width = 6, 
       height = 4)

#Extract top 10 coefficients by absolute value
beta_hat_std = extract_std_coefs(lasso_fit, listings_train)
top_10_features = beta_hat_std %>% 
  filter(coefficient != 0) %>% 
  arrange(desc(abs(coefficient))) %>%
  head(20)
write_csv(x = top_10_features, file = "results/top10_features.csv")
