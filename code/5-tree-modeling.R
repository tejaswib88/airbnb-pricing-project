library(randomForest)

listings = read.csv("data/clean/cleaned_listings.csv")
listings = listings %>% mutate_if(is.character,as.factor)

set.seed(1)
#Generate training samples & split
train_samples = sample(1:nrow(listings), 0.8*nrow(listings))
listings_train = listings %>% filter(row_number() %in% train_samples)

#Fit random forest
rf_fit = randomForest(price ~ ., data = listings_train)

#Tune mtry and save image
mvalues = seq(1,30, by = 2)
oob_errors = numeric(length(mvalues))
ntree = 500

for(idx in 1:length(mvalues)){
  m = mvalues[idx]
  rf_fit = randomForest(price ~ ., mtry = m, data = listings_train)
  oob_errors[idx] = rf_fit$mse[ntree]
}
errors = tibble(m = mvalues, oob_err = oob_errors) 

p = errors %>%
  ggplot(aes(x = m, y = oob_err)) + 
  geom_line() + geom_point() + 
  scale_x_continuous(breaks = mvalues) +
  theme_bw()

ggsave(filename = "results/rf-mtry-plot.png", 
       plot = p, 
       device = "png", 
       width = 6, 
       height = 4)

#Find the optimal value of mtry
optimal_mtry = errors %>% 
  filter(oob_err == min(errors$oob_err)) %>%
  select(m)
optimal_mtry = optimal_mtry$m[1]

#Create final model using tuned mtry
rf_finalfit = randomForest(price ~ ., mtry = optimal_mtry, data = listings_train, 
                           importance = TRUE)
save(rf_finalfit, file = "results/rf_finalfit.Rda")


