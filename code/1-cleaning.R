# load libraries
library(lubridate)
library(tidyverse)
library(dplyr)

# load raw case data
listings = read.csv(file = "data/raw/listings.csv")

#drop unecessary columns
listings = listings %>%
  select(-listing_url, -scrape_id, -last_scraped, -name, -description, 
         -neighborhood_overview, -picture_url, -host_id, -host_url, -host_name, 
         -host_thumbnail_url, -host_picture_url, -neighbourhood_cleansed, 
         -neighbourhood_group_cleansed, -bathrooms, -minimum_maximum_nights,
         -minimum_minimum_nights, -maximum_minimum_nights, 
         -maximum_maximum_nights, -minimum_nights_avg_ntm, 
         -maximum_nights_avg_ntm, -calendar_updated, -first_review, 
         -last_review, -license, 
         -calculated_host_listings_count_entire_homes,
         -calculated_host_listings_count_private_rooms, 
         -calculated_host_listings_count_shared_rooms,
         -calculated_host_listings_count,
         -neighbourhood, -latitude, -longitude,
         -host_neighbourhood, -id, -has_availability,
         -calendar_last_scraped,
         -host_location,
         -property_type,
         -host_total_listings_count)

#Create feature for length of about description and drop host_about
listings = listings %>% 
  mutate(about_length = str_count(host_about, "\\S+")) %>%
  select(-host_about)

#Create feature for amount of days host has been on airbnb, and drop host_since
collection_date = as.Date("2021-10-18")

listings$days_hosting = unlist(lapply(
  listings$host_since, 
  function(x) length(seq(from = as.Date(x), 
                         to = collection_date, 
                         by = 'day')) - 1))

listings = listings %>% select(-host_since)

#Drop N?A rows for response time, rate, and acceptance rate since we can't 
#make reasonable adjustments

listings = listings %>% filter(host_response_time != "N/A" &
                                 host_response_rate != "N/A" &
                                 host_acceptance_rate != "N/A")

#Create feature corresponding to number of verifications
listings = listings %>% 
  mutate(num_verifications = str_count(host_verifications, ",") + 1) %>%
  select(-host_verifications)

#Create feature corresponding to number of amenities
listings = listings %>% 
  mutate(num_amenities = str_count(amenities, ",") + 1) %>%
  select(-amenities)

#Convert the price column into a double and drop N/A
listings = listings %>% 
  mutate(price = gsub("\\$", "", price)) %>%
  mutate(price = as.double(price))
listings = listings %>% drop_na()

#Convert percentage character columns into doubles
listings = listings %>%
  mutate(host_response_rate = 
           as.double(gsub("\\%", "", host_response_rate))/100,
         host_acceptance_rate = 
           as.double(gsub("\\%", "", host_acceptance_rate))/100)

#Convert character columns into factors
listings = listings %>% mutate_if(is.character,as.factor)

#Drop N/A
listings = listings %>% drop_na()

#Write the cleaned tibble to a new CSV
write_csv(x = listings, file = "data/clean/cleaned_listings.csv")


