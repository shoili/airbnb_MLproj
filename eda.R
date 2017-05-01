setwd("~/Documents/airbnb_MLproj")
library(dplyr)

# read data
calendar <- read.csv("calendar.csv", strip.white = T, stringsAsFactors = F)
listings <- read.csv("listings.csv", strip.white = T, stringsAsFactors = F)
reviews <- read.csv("reviews.csv", strip.white = T, stringsAsFactors = F)

# clean calendar table
table(calendar$available)

available_listings <- calendar %>%
  filter(available == "t")

available_listings$price_num <- as.numeric(sub("\\$","", available_listings$price))
summary(available_listings$price_num)

available_listings <- available_listings[!is.na(available_listings$price_num), ]
write.csv(available_listings, file = "available_listings.csv")

rm(calendar)

# listings
colnames(listings)
# half the columns are text, will need mining. half are more of less straight up features, just convert t/f to 1/0

# reviews
colnames(reviews)
# generate features from the comments