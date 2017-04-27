library(dplyr)

available <- read.csv('available_listings.csv')
listings <- read.csv("listings_final.csv")
reviews <- read.csv("Data/reviewsadj.csv")

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
