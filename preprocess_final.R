library(dplyr)

# read data
listings <- read.csv("listings.csv", strip.white = T, stringsAsFactors = F)

listings <- listings %>%
  select(-listing_url, -scrape_id, -last_scraped, -name)

listings$downtown <- as.numeric(grepl("downtown", tolower(listings$summary)) & grepl("downtown", tolower(listings$description)))
listings$kitchen <- as.numeric(grepl("kitchen", tolower(listings$summary)))
listings$restaurants <- as.numeric(grepl("restaurants", tolower(listings$summary)))
listings$laundry <- as.numeric(grepl("laundry", tolower(listings$description)))
listings$harvard <- as.numeric(grepl("harvard", tolower(listings$description)) & grepl("harvard", tolower(listings$summary)))
listings$bus <- as.numeric(grepl("bus", tolower(listings$transit)))
listings$subway <- as.numeric(grepl("subway", tolower(listings$transit)))
listings$airport <- as.numeric(grepl("airport", tolower(listings$transit)))
listings$nosmoking <- as.numeric(grepl("no smoking", tolower(listings$house_rules)))

listings <- listings %>%
  select(-summary, -space, -description, -experiences_offered, 
         -neighborhood_overview, -transit, -notes, -interaction, 
         -latitude, -longitude, -access, -house_rules, -host_id,
         -host_url, -host_name, -host_since, -host_location, -host_about,
         -host_neighbourhood, -street, -neighbourhood, -neighbourhood_group_cleansed,
         -city, -state, -market, -smart_location, -country_code, -country, -square_feet,
         -calendar_updated, -has_availability, -calendar_last_scraped, -first_review,
         -last_review, -license, -jurisdiction_names)

listings$thumbnail_url <- ifelse(listings$thumbnail_url != "", 1, 0)
listings$medium_url <- ifelse(listings$medium_url != "", 1, 0)
listings$picture_url <- ifelse(listings$picture_url != "", 1, 0)
listings$xl_picture_url <- ifelse(listings$xl_picture_url != "", 1, 0)

listings$host_acceptance_rate <- as.numeric(sub("%", "", listings$host_acceptance_rate))
listings$host_response_rate <- as.numeric(sub("%", "", listings$host_response_rate))
listings$host_response_time <- as.factor(listings$host_response_time)

listings$host_is_superhost <- ifelse(listings$host_is_superhost == "t", 1, 0)

listings$host_thumbnail_url <- ifelse(listings$host_thumbnail_url != "", 1, 0)
listings$host_picture_url <- ifelse(listings$host_picture_url != "", 1, 0)

listings$host_verifications <- lengths(strsplit(listings$host_verifications, split=","))

listings$host_has_profile_pic <- ifelse(listings$host_has_profile_pic == "t", 1, 0)
listings$host_identity_verified <- ifelse(listings$host_identity_verified == "t", 1, 0)

listings$is_location_exact <- ifelse(listings$is_location_exact == "t", 1, 0)

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

listings <- cbind(listings, amen_cat)
listings$amenities_list <- vapply(listings$amenities_list, paste, collapse = ", ", character(1L))
rm(amen_cat)

listings$price <- NULL
listings$weekly_price <- NULL
listings$monthly_price <- NULL
listings$security_deposit <- as.numeric(sub("\\$","", listings$security_deposit))
listings$security_deposit[is.na(listings$security_deposit)] <- 0

listings$cleaning_fee <- as.numeric(sub("\\$","", listings$cleaning_fee))
listings$cleaning_fee[is.na(listings$cleaning_fee)] <- 0
listings$extra_people <- as.numeric(sub("\\$","", listings$extra_people))
listings$extra_people[is.na(listings$extra_people)] <- 0

listings$requires_license <- ifelse(listings$requires_license == "t", 1, 0)
listings$instant_bookable <- ifelse(listings$instant_bookable == "t", 1, 0)

listings$require_guest_profile_picture <- ifelse(listings$require_guest_profile_picture == "t", 1, 0)
listings$require_guest_phone_verification <- ifelse(listings$require_guest_phone_verification == "t", 1, 0)

write.csv(listings, "listings_final.csv")
