#setwd('Documents/Georgia_Tech/Spring_2017/CSE_6740/airbnb/')
#setwd('/Users/Stevenstuff/airbnb_MLproj/')
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

# todo 14, 19, 30: converting t/f columns to 0/1
listings$host_is_superhost <- ifelse(listings$host_is_superhost == "t", 1, 0)
listings$host_has_profile_pic <- ifelse(listings$host_has_profile_pic == "t", 1, 0)
listings$host_identity_verified <- ifelse(listings$host_identity_verified == "t", 1, 0)
listings$is_location_exact <- ifelse(listings$is_location_exact == "t", 1, 0)
listings$requires_license <- ifelse(listings$requires_license == "t", 1, 0)
listings$instant_bookable <- ifelse(listings$instant_bookable == "t", 1, 0)
listings$require_guest_profile_picture <- ifelse(listings$require_guest_profile_picture == "t", 1, 0)
listings$require_guest_phone_verification <- ifelse(listings$require_guest_phone_verification == "t", 1, 0)

# todo 6, 8, 15: transit 0/1 if field is there, 1 means good access to transportation
listings$transit_bin <- ifelse(listings$transit != "", 1, 0)
listings$thumbnail_bin <- ifelse(listings$thumbnail_url != "", 1, 0)
listings$medium_bin <- ifelse(listings$medium_url != "", 1, 0)
listings$picture_bin <- ifelse(listings$picture_url != "", 1, 0)
listings$xl_picture_bin <- ifelse(listings$xl_picture_url != "", 1, 0)
listings$host_thumbnail_bin <- ifelse(listings$host_thumbnail_url != "", 1, 0)
listings$host_picture_bin <- ifelse(listings$host_picture_url != "", 1, 0)

# todo 16: host neighborhood: factorize it since there's 53 neighborhoods
# todo 17: leave host_listings_count and host_listings_total_count as is
listings$host_neighbourhood <- as.factor(listings$host_neighbourhood)

# todo 18: host verifications: count of items in list
listings$host_verifications_count <- lengths(strsplit(listings$host_verifications, split=","))

# todo 20: extract zipcode from street
zip = c()
for(i in 1:length(listings$street)) {
  m <- regexpr("\\d{5}", listings$street[i], perl=TRUE)
  match <- regmatches(listings$street[i],m)
  ifelse(length(match) > 0, zip <- c(zip, regmatches(listings$street[i], m)), zip <- c(zip, NA))
}
listings$zipcode <- zip

################ todo 26: amenities ###################
amen_list = c()
for(i in 1:length(listings$amenities)) {
  amends <- strsplit(listings$amenities[i], split=",")
  for(j in 1:length(amends[[1]])) {
    amends[[1]][j] <- gsub('[^a-zA-Z0-9 ]', '', amends[[1]][j])
  }
  amen_list <- c(amen_list, amends)
}
# remove missing translations from amenities
for(i in 1:length(amen_list)){
  for(j in 1:length(amen_list[i][[1]])){
    ifelse(grepl("translation", amen_list[i][[1]][j]) == TRUE, 
                 amen_list[i][[1]][j] <- NA, 
                 amen_list[i][[1]][j] <- amen_list[i][[1]][j])
    ifelse(amen_list[i][[1]][j] == "",
           amen_list[i][[1]][j] <- NA,
           amen_list[i][[1]][j] <- amen_list[i][[1]][j])
  }
  amen_list[i] <- lapply(amen_list[i], function(x) x[!is.na(x)])
}
# amen_list[i] has list of amenities for that listing
listings$amenities_list <- amen_list

# 43 total amenities across all the listings
all_amenities <- unique(unlist(amen_list))

# amenity counts for each listing
amenity_counts = c()
for(i in 1:length(amen_list)){
  count <- length(amen_list[i][[1]])
  amenity_counts <- c(amenity_counts, count)
}
listings$amenity_counts <- amenity_counts

## make 0/1 variables for each of the 43 amenities ## 
amen_cat <- as.data.frame(matrix(0, ncol=length(all_amenities), nrow=length(amen_list)))
names(amen_cat) <- all_amenities
for(i in 1:dim(amen_cat)[2]){
  for(j in 1:dim(amen_cat)[1]){
    if(all_amenities[i] %in% amen_list[[j]]){
      amen_cat[j,i] <- 1
    }
  }
}

############# final listings.csv with every amenity as 1/0, 
############# all other t/f converted to 1/0
############# columns is 106 + 43 = 149 for the 43 amenities
amenities_listings <- cbind(listings, amen_cat)
dim(amenities_listings)

# flatten listings$amenities_list into char string to write to csv
amenities_listings$amenities_list <- vapply(listings$amenities_list, paste, collapse = ", ", character(1L))
for(i in 1:dim(listings)[2]){
  print(class(listings[,i]))
}

write.csv(amenities_listings, 'amenities_listings.csv')
