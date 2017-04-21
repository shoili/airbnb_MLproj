library(dplyr)

available <- read.csv('available_listings.csv')
listings <- read.csv("listings_final.csv")
reviews <- read.csv("Data/reviewsadj.csv")

avail_listings <- merge(listings, available, by.x="id", by.y="listing_id", all.y=TRUE)
avail_listings_rev <- merge(avail_listings, reviews, by.x="id", by.y="listing_id", 
                            all.x=TRUE)

avail_listings2 <- merge(listings, available, by.x="id", by.y="listing_id", 
                        all = TRUE)
avail_listings_rev2 <- merge(avail_listings2, reviews, by.x="id", by.y="listing_id", 
                            all = TRUE)

avail_listings_rev <- avail_listings_rev %>%
  select(-X.x, X, X.y)

avail_listings_rev2 <- avail_listings_rev2 %>%
  select(-X.x, X, X.y)

write.csv(avail_listings_rev2, "dataset1.csv")
write.csv(avail_listings_rev, "dataset2.csv")
