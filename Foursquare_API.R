install.packages("httr")

library(httr)

listing_data <- read.csv("./data/cleaned_listings_data.csv")

clientId <- "********** Use foursquare client Id **************"
clientSecret <- "********** Use foursquare client Secret **************"
api_version <- 20190425 
radius <- 200
api_url <- "https://api.foursquare.com/v2/venues/search"

# category Ids of grocery, busstop, cafe, pub etc,.
GrocerySuperMarket_category = "4bf58dd8d48988d118951735,52f2ab2ebcbc57f1066b8b46"
BusStop_category = "4bf58dd8d48988d1fe931735,52f2ab2ebcbc57f1066b8b4f"
Cafe_category = "4bf58dd8d48988d16d941735,4bf58dd8d48988d1e0931735"
BarPub_category = "4bf58dd8d48988d116941735"

getVenuesNearBy <- function(latitude, longitude,category){
  url <- paste(api_url, 
               "?client_id=", clientId, 
               "&client_secret=", clientSecret,
               "&categoryId=", category,
               "&ll=", latitude, "," , longitude,
               "&radius=", radius,
               "&v=", api_version, sep="")
  resp <- GET(url) 
  print(url)
  json_resp <- content(resp, as="parsed")
  if(length(json_resp$response$venues)>0) 
  {
    return(TRUE)
  }
  else
  {
    return(FALSE)
  }
}
  

listing_data$GrocerySuperMarket = mapply(getVenuesNearBy,listing_data$latitude,listing_data$longitude,GrocerySuperMarket_category) *1
listing_data$BusStop = mapply(getVenuesNearBy,listing_data$latitude,listing_data$longitude,BusStop_category) *1
listing_data$Cafe = mapply(getVenuesNearBy,listing_data$latitude,listing_data$longitude,Cafe_category)*1
listing_data$BarPub = mapply(getVenuesNearBy,listing_data$latitude,listing_data$longitude,BarPub_category)*1

write.csv(listing_data, "./data/final_data.csv")
