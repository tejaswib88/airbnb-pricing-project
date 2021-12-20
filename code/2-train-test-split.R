listings = read.csv("data/clean/cleaned_listings.csv")
listings = listings %>% mutate_if(is.character,as.factor)

set.seed(1)
#Generate training samples & split
train_samples = sample(1:nrow(listings), 0.8*nrow(listings))
listings_train = listings %>% filter(row_number() %in% train_samples)
listings_test = listings %>% filter(!(row_number() %in% train_samples))

#Write the training and test files to new CSVs
write_csv(x = listings_train, file = "data/clean/listings_train.csv") 
write_csv(x = listings_test, file = "data/clean/listings_test.csv")

