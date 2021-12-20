# load libraries
library(kableExtra)                     # for printing tables
library(cowplot)                        # for side by side plots
library(lubridate)                      # for dealing with dates
library(maps)                           # for creating maps
library(tidyverse)

#read the training data
data = read.csv("data/clean/listings_train.csv")
data = data %>% mutate_if(is.character,as.factor)

#Distribution of price
p = data %>%
  ggplot(aes(x = price)) +
  geom_histogram() +
  geom_vline(xintercept = median(data$price),
             color = "blue",
             linetype = "dashed") +
  labs(x = "Price", y = "Frequency") +
  theme_bw()

# save the histogram
ggsave(filename = "results/price-histogram.png", 
       plot = p, 
       device = "png", 
       width = 5, 
       height = 3)

# Top 10 Listings by Price 
p = data %>% 
  arrange(desc(price)) %>% 
  head(10) %>% 
  select(price, host_response_time, host_response_rate, host_is_superhost)

write_csv(x = p, file = "results/top-10-prices.csv")

#Response time against price
p = data %>%
  ggplot(aes(x = host_response_time, y = price)) +
  geom_boxplot() +
  labs(x = "Host Response Time", y = "Price") +
  theme_bw()

# save the boxplot
ggsave(filename = "results/price-response.png", 
       plot = p, 
       device = "png", 
       width = 5, 
       height = 3)

#Response rate against price
p = data %>%
  ggplot(aes(x = host_response_rate, y = price)) +
  geom_point() +
  labs(x = "Host Response Rate", y = "Price") +
  theme_bw()

#Save the scatterplot
ggsave(filename = "results/price-response-rate.png", 
       plot = p, 
       device = "png", 
       width = 5, 
       height = 3)

#Superhost and price
p = data %>%
  ggplot(aes(x = host_is_superhost, y = price)) +
  geom_boxplot() +
  labs(x = "Is Host a Superhost?", y = "Price") +
  theme_bw()

#Save the boxplot
ggsave(filename = "results/price-superhost.png", 
       plot = p, 
       device = "png", 
       width = 5, 
       height = 3)

#Verified? and price
p = data %>%
  ggplot(aes(x = host_identity_verified, y = price)) +
  geom_boxplot() +
  labs(x = "Is Host Verified?", y = "Price") +
  theme_bw()

#Save the boxplot
ggsave(filename = "results/price-verified.png", 
       plot = p, 
       device = "png", 
       width = 5, 
       height = 3)

#Instant bookability and price
p = data %>%
  ggplot(aes(x = instant_bookable, y = price)) +
  geom_boxplot() +
  labs(x = "Is Property Instantly Bookable?", y = "Price") +
  theme_bw()

#Save the boxplot
ggsave(filename = "results/price-instant.png", 
       plot = p, 
       device = "png", 
       width = 5, 
       height = 3)

#Room type and price
p = data %>%
  ggplot(aes(x = room_type, y = price)) +
  geom_boxplot() +
  labs(x = "Room Type", y = "Price") +
  theme_bw()


#Save the boxplot
ggsave(filename = "results/price-room-type.png", 
       plot = p, 
       device = "png", 
       width = 5, 
       height = 3)

#Number of reviews and price
p = data %>%
  ggplot(aes(x = number_of_reviews, y = price)) +
  geom_point() +
  labs(x = "Number of Reviews", y = "Price") +
  theme_bw()

#Save the scatterplot
ggsave(filename = "results/price-num-reviews.png", 
       plot = p, 
       device = "png", 
       width = 5, 
       height = 3)

#Reviews rating and price
p = data %>%
  ggplot(aes(x = review_scores_rating, y = price)) +
  geom_point() +
  labs(x = "Rating", y = "Price") +
  theme_bw()

#Save the scatterplot
ggsave(filename = "results/price-review-score.png", 
       plot = p, 
       device = "png", 
       width = 5, 
       height = 3)

#Number of verifications and price
p = data %>%
  ggplot(aes(x = price, y = as.factor(num_verifications))) +
  geom_boxplot() +
  labs(x = "Price", y = "Number of Verifications") +
  theme_bw()

#Save the boxplot
ggsave(filename = "results/price-num-verifications.png", 
       plot = p, 
       device = "png", 
       width = 5, 
       height = 3)

#Number of amenities and price
p = data %>%
  ggplot(aes(x = num_amenities, y = price)) +
  geom_point() +
  labs(x = "Number of Amenities", y = "Price") +
  theme_bw()

#Save the scatterplot
ggsave(filename = "results/price-num-amenities.png", 
       plot = p, 
       device = "png", 
       width = 5, 
       height = 3)


