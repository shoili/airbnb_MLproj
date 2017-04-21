listings <- read.csv("amenities_listings.csv")

listings$host_acceptance_rate <- as.numeric(sub("%", "", listings$host_acceptance_rate))
listings$host_response_rate <- as.numeric(sub("%", "", listings$host_response_rate))
listings$host_response_time <- as.factor(listings$host_response_time)

# 25. bed type is already a factor 
# 24. accomodates bathrooms bedrooms beds checked that these are integer 

# 23. property_type and room_type are factors 

# 21. nbd is a factor

# 22. city state zipcode country lat long

# 27. square feet. keep as is numeric
# 28. remove all the prices and keep as possible alternate responses probably
listings$price <- NULL
listings$weekly_price <- NULL
listings$monthly_price <- NULL
listings$security_deposit <- as.numeric(sub("\\$","", listings$security_deposit))
listings$security_deposit[is.na(listings$security_deposit)] <- 0

# 29. min/max night keep as is

# 31. cancellation policy. factor

