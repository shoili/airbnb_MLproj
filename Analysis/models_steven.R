# run joins.R first

# use the inner joined dataset, "inner"
inner_nona <- na.omit(inner)

# amenities/amenities_list was collected via count/categorical vars
# price is reflected in price_num
# X.x, X.y, X are from the joins/represent row numbers

# available, requires_license, host_thumbnail_url, host_picture_url, picture_url
# are the same value for every row
inner_nona <- subset(final_nona, select = -c(price, X.x, amenities, 
                                             amenities_list, X.y, available, 
                                             X, requires_license, host_thumbnail_url,
                                             host_picture_url, picture_url))


