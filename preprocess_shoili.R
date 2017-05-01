listings <- read.csv("listings.csv")

listings$host_acceptance_rate <- as.numeric(sub("%", "", listings$host_acceptance_rate))
listings$host_response_rate <- as.numeric(sub("%", "", listings$host_response_rate))
listings$host_response_time <- as.factor(listings$host_response_time)

listings$notes <- NULL
listings$neighborhood_overview <- NULL

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

# 3

library(tm)
library(RWeka)

BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))

# corp <- Corpus(VectorSource(listings$summary))
# corp <- Corpus(VectorSource(listings$space))
# corp <- Corpus(VectorSource(listings$description))
corp <- Corpus(VectorSource(listings$transit))

corp <- tm_map(corp, tolower)
corp <- tm_map(corp, removePunctuation)
corp <- tm_map(corp, removeNumbers)
corp <- tm_map(corp, removeWords, stopwords("english"))

tdm <- TermDocumentMatrix(corp, control = list(tokenize = BigramTokenizer))

# tdm <- removeSparseTerms(tdm, 0.99)
# print("----")
# print("tdm properties")
# str(tdm)
# tdm_top_N_percent = tdm$nrow / 100 * 20

# inspect(tdm[1:20, 1:10])
findFreqTerms(tdm, lowfreq = 500)

# publictransport <- as.numeric(grepl("transportation", tolower(listings$summary)))
downtown <- as.numeric(grepl("downtown", tolower(listings$summary)) & grepl("downtown", tolower(listings$description)))
kitchen <- as.numeric(grepl("kitchen", tolower(listings$summary)))
restaurants <- as.numeric(grepl("restaurants", tolower(listings$summary)))
laundry <- as.numeric(grepl("laundry", tolower(listings$description)))
harvard <- as.numeric(grepl("harvard", tolower(listings$description)) & grepl("harvard", tolower(listings$summary)))
bus <- as.numeric(grepl("bus", tolower(listings$transit)))
subway <- as.numeric(grepl("subway", tolower(listings$transit)))
airport <- as.numeric(grepl("airport", tolower(listings$transit)))
nosmoking <- as.numeric(grepl("no smoking", tolower(listings$house_rules)))

listings$space <- NULL
listings$summary <- NULL
listings$name <- NULL
listings$description <- NULL
listings$experiences_offered <- NULL
listings$transit <- NULL
listings$notes <- NULL
listings$interaction <- NULL
listings$latitude <- NULL
listings$longitude <- NULL
