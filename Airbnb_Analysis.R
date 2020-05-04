install.packages("Amelia")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("wordcloud")
install.packages("ggwordcloud")
install.packages("forcats")
library(Amelia)
library(dplyr)
library(ggplot2)
library(wordcloud)
library(ggwordcloud)
library(forcats)
# Detailed listings data for New York
listing_data <- read.csv("./data/listings-summary.csv")

# dataset has 51097 rows and 106 columns
dim(listing_data)

# keeping columns relevant to the problem
columns_to_keep <- c('host_is_superhost','host_id', 'host_since', 'last_scraped',
                     'host_total_listings_count', 'host_has_profile_pic', 'host_identity_verified', 'neighbourhood_cleansed',
                     'neighbourhood_group_cleansed', 'latitude', 'longitude', 'is_location_exact', 'property_type',
                     'room_type', 'accommodates', 'bathrooms', 'bedrooms', 'beds', 'bed_type', 'amenities',
                     'price', 'security_deposit', 'cleaning_fee', 'guests_included',
                     'extra_people', 'minimum_nights', 'maximum_nights', 'availability_365', 'number_of_reviews',
                     'review_scores_rating','reviews_per_month','review_scores_accuracy', 'review_scores_cleanliness', 'review_scores_checkin',
                     'review_scores_communication','review_scores_location', 'review_scores_value',
                     'instant_bookable', 'cancellation_policy', 'require_guest_profile_picture',
                     'require_guest_phone_verification', 'calculated_host_listings_count', 'transit')


initial_df <- listing_data[columns_to_keep]

dim(initial_df)

# 
# 2. Data cleaning and pre-processing
# 

# we will apply conversion / cleaning to below variables

initial_df$host_is_superhost <- as.numeric(ifelse(initial_df$host_is_superhost == 't', 1, 0))
initial_df$host_has_profile_pic <- as.numeric(ifelse(initial_df$host_has_profile_pic == 't', 1, 0))
initial_df$host_identity_verified <- as.numeric(ifelse(initial_df$host_identity_verified == 't', 1, 0))
initial_df$instant_bookable <- as.numeric(ifelse(initial_df$instant_bookable == 't', 1 ,0))
initial_df$require_guest_profile_picture <- as.numeric(ifelse(initial_df$require_guest_profile_picture == 't', 1 ,0))
initial_df$require_guest_phone_verification <- as.numeric(ifelse(initial_df$require_guest_phone_verification == 't', 1, 0))

# Below variables are in currency format, apply regular expression and converting them to numeric

initial_df$price <- as.numeric(gsub('[$,]', '', initial_df$price))
initial_df$extra_people <- as.numeric(gsub('[$,]', '', initial_df$extra_people))
initial_df$cleaning_fee <- as.numeric(gsub('[$,]', '', initial_df$cleaning_fee))
initial_df$security_deposit <- as.numeric(gsub('[$,]', '', initial_df$security_deposit))

#
# Handling "NA"s/Missing Values in Dataset
#

# View Missing values distribution in Dataset
missmap(initial_df,main = "Missing values")

sum(is.na(initial_df$security_deposit)) # security deposit: 17580 observations with NAs
sum(is.na(initial_df$cleaning_fee)) # cleaning fee: 10743 observations with NAs
sum(is.na(initial_df$review_scores_rating))

# cleaning_fee and security_deposit has NA, we will input 0 for these columns based on the 
# assumption that there is no cleaning fee or security deposit

initial_df$security_deposit[is.na(initial_df$security_deposit)] <- 0
initial_df$cleaning_fee[is.na(initial_df$cleaning_fee)] <- 0

# we have 30% of data with missing values in review_scores_rating 
df_without_NAs <- initial_df[!is.na(initial_df$review_scores_rating),]

# 'review_scores_rating' Imputation
#  1. If review_scores_rating is missing, check if missing lists was rated on other listings and get the average
#  2. If Host was not rated, then impute rating with Mean rating of the Neighbourhood  

df_with_rating_NA = initial_df[is.na(initial_df$review_scores_rating),]
df_without_rating_NA = initial_df[!is.na(initial_df$review_scores_rating),]

# Finding Average rating based on HostID
df_without_rating_NA %>% 
  group_by(host_id) %>% 
  summarise(avg_rating=mean(review_scores_rating)) %>%
  select(host_id, avg_rating) -> host_mean_rating

# Finding Average rating based on Neighbourhood
df_without_rating_NA %>% 
  group_by(neighbourhood_cleansed) %>% 
  summarise(avg_rating=mean(review_scores_rating)) %>%
  select(neighbourhood_cleansed, avg_rating) -> neighbourhood_mean_rating

dim(initial_df)

#Function to compute missing rating values based on HostId and Neighbourhood
rating_fun <- function(review_scores_rating, host_id, neighbourhood_cleansed ){
  if(is.na(review_scores_rating)){
    if(sum(host_mean_rating$host_id == host_id) > 0){
      review_scores_rating <- host_mean_rating[
        host_mean_rating$host_id == host_id,
        ]$avg_rating
    } else{
      review_scores_rating <- neighbourhood_mean_rating[
        neighbourhood_mean_rating$neighbourhood_cleansed == neighbourhood_cleansed,
        ]$avg_rating
    }
  }
  return(review_scores_rating);
}

#Impute review_scores_rating column
#initial_df$review_scores_rating <- mapply(rating_fun, as.numeric(initial_df$review_scores_rating),initial_df$host_id, initial_df$neighbourhood_cleansed)
initial_df$review_scores_rating[is.na(initial_df$review_scores_rating)]<-median(na.omit(initial_df$review_scores_rating))
initial_df$review_scores_accuracy[is.na(initial_df$review_scores_accuracy)] <- 0
initial_df$review_scores_cleanliness[is.na(initial_df$review_scores_cleanliness)] <- 0
initial_df$review_scores_checkin[is.na(initial_df$review_scores_checkin)] <- 0
initial_df$review_scores_communication[is.na(initial_df$review_scores_communication)] <- 0
initial_df$review_scores_location[is.na(initial_df$review_scores_location)] <- 0
initial_df$review_scores_value[is.na(initial_df$review_scores_value)] <- 0


# Replacing non-alphanumeric characters with underscores to prevent code errors
initial_df$bed_type <- gsub("[^[:alnum:]]", "_", initial_df$bed_type)
initial_df$neighbourhood_cleansed <- gsub("[^[:alnum:]]", "_", initial_df$neighbourhood_cleansed)
initial_df$room_type <- gsub("[^[:alnum:]]", "_", initial_df$room_type)
initial_df$neighbourhood_group_cleansed <- gsub("[^[:alnum:]]", "_", initial_df$neighbourhood_group_cleansed)
initial_df$property_type <- gsub("[^[:alnum:]]", "_", initial_df$property_type)

# some cleaning of property type is required, as there are large number of categories with few listings
unique(initial_df$property_type)

initial_df %>%
  group_by(property_type) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count)) %>%
  ungroup() -> p

# Replacing categories that are types of houses or apartments or hotel
initial_df$property_type <- sapply(initial_df$property_type, function(type){
  value <- 'other';
  if(type %in% c('Serviced_apartment', 'Loft', 'Aparthotel', 'Apartment')){
    value <- 'Apartment'
  } else if(type %in% c('Bungalow', 'Cottage', 'House', 'Townhouse', 'Villa', 'Tiny_house', 'Guesthouse')){
    value <- 'House'
  } else if(type %in% 'Condominium'){
    value <- 'Condominium'
  }else if(type %in% c('Boutique_hotel', 'Hotel')){
    value <- 'Hotel'
  }
  return(value);
})


# bedrooms, beds, bathrooms has missing values, imputing median

initial_df$bedrooms[is.na(initial_df$bedrooms)] <- median(na.omit(initial_df$bedrooms))
initial_df$beds[is.na(initial_df$beds)] <- median(na.omit(initial_df$beds))
initial_df$bathrooms[is.na(initial_df$bathrooms)] <- median(na.omit(initial_df$bathrooms))


# adding new calculated variables
# there are 6 observations with missing values in host_since, dropping the observations

initial_df <- initial_df[!is.na(initial_df$host_since), ]
initial_df$host_since <- as.Date(initial_df$host_since)

initial_df$hosting_duration <- as.numeric(difftime(initial_df$last_scraped, initial_df$host_since, unit='days'))
initial_df$price_per_person <- initial_df$price / initial_df$accommodates

initial_df$host_since <- NULL
initial_df$last_scraped <- NULL
initial_df <- initial_df[!is.na(initial_df$hosting_duration), ]
dim(initial_df)

# Amenities is a list of additional features in the property i.e whether it has a TV or Wifi.
initial_df$amenities <- as.character(initial_df$amenities)

# bathroom bedroom bed count 0, accomodate has maximum of 16 where median val is 2. price 
# is 0 for few guests included has maximum value of 16 where median is 1 
# minimum nights maximum value 1250 where median is 2 , max nights has few error

#bathroom
summary(initial_df$bathrooms)
initial_df$bathrooms[initial_df$bathrooms==0]<-NA
initial_df$bathrooms[is.na(initial_df$bathrooms)]<-median(na.omit(initial_df$bathrooms))
#making beds=1 where bed type = airbed and real bed.
initial_df$beds[(initial_df$bed_type=="Real_Bed")&(initial_df$beds==0)]<-median(initial_df$beds)
initial_df$beds[(initial_df$bed_type=="Airbed")&(initial_df$beds==0)]<-median(initial_df$beds)
summary(initial_df$price)
initial_df<-initial_df[!(initial_df$price==0),]

initial_df$reviews_per_month[is.na(initial_df$reviews_per_month)] <- 0

dim(initial_df)

# Outliers in price
quantile(initial_df$price)

boxplot_price <- ggplot(initial_df, aes( ,price)) +
  geom_boxplot(col="red")

boxplot_price + coord_flip() 

# 99% of observations are in the price range 49$ - 850$ 
quantile(initial_df$price, c(0.99))

# removed the outliers for comparability reasons
initial_df <- initial_df %>% filter(price > 49 & price <= 850)

dim(initial_df)

essential_amenties <- '{"Toilet Paper","Body soap","Bath towel","Extra pillows and blankets","Bed linens"}'

# There are 47 observation with no details of amenities, value as {}
# For a property to be airbnb listing, it should have essential amenities
# Essential amenities are the basic items that a guest expects in order to have a comfortable stay.
# https://www.airbnb.com/help/article/2343/what-are-essential-amenities

# replaced missing amenities with basic essential amenities 
initial_df$amenities[initial_df$amenities=="{}"] <- essential_amenties
