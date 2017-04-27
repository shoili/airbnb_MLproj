library(dplyr)

available <- read.csv('available_listings.csv')
available$X <- NULL
listings <- read.csv("listings_final.csv")
listings$X <- NULL
reviews <- read.csv("Data/reviewsadj.csv")
reviews$X <- NULL

avail <- merge(listings, available, by.x="id", by.y="listing_id", all.y=TRUE)
left <- merge(avail, reviews, by.x="id", by.y="listing_id", 
                            all.x=TRUE)

avail_inner <- merge(listings, available, by.x="id", by.y="listing_id")
inner <- merge(avail_inner, reviews, by.x="id", by.y="listing_id")

# left joined dataset
left <- left %>%
  select(-X.x, X, X.y)

# inner joined dataset
inner <- inner %>%
  select(-X.x, X, X.y)

#write.csv(left, "dataset1.csv")
#write.csv(inner, "dataset2.csv")

##################################################
# de duplicated dataset without time series part


available2 <- available %>%
  group_by(listing_id) %>%
  mutate(avgprice = mean(price_num),
         pricevar = var(price_num)) %>%
  select(listing_id, avgprice, pricevar)

available2 <- unique(available2)

available2_listings <- merge(listings, available2, by.x="id", by.y="listing_id", all.x = F, all.y = F)
finaldata <- merge(available2_listings, reviews, by.x="id", by.y="listing_id")

