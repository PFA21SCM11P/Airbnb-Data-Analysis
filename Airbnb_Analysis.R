install.packages("Amelia")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("wordcloud")
install.packages("ggwordcloud")
install.packages("forcats")
install.packages("fastDummies")
install.packages("tm")
install.packages("SnowballC")
install.packages("wordcloud") 
install.packages("RColorBrewer")
install.packages("qdapTools")

library(Amelia)
library(dplyr)
library(ggplot2)
library(wordcloud)
library(ggwordcloud)
library(forcats)
library(fastDummies)
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
library(qdapTools)
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


# 3. Exploratory Data Analysis

initial_df %>%
  group_by(neighbourhood_cleansed) %>%
  summarize(num_listings = n(), 
            borough = unique(neighbourhood_group_cleansed)) %>%
  top_n(n = 10, wt = num_listings) %>%
  ggplot(aes(x = fct_reorder(neighbourhood_cleansed, num_listings), 
             y = num_listings, fill = borough)) +
  geom_col() +
  coord_flip() +
  theme(legend.position = "bottom") +
  labs(title = "Top 10 Neighbourhoods by no. of listings",
       x = "Neighborhood", y = "No. of listings")


ggplot(initial_df, aes(price)) +
  geom_boxplot(col="red")

ggplot(initial_df, aes(price,minimum_nights)) +
  geom_boxplot(col="red")

ggplot(initial_df, aes(price,minimum_nights)) +
  geom_boxplot(col="red")

plot(initial_df$price, initial_df$minimum_nights, xlim=c(0, 200), ylim=c(0, 2000), main="With Outliers", xlab="Price", ylab="Minimum Nights", pch="*", col="red", cex=2)

abline(lm(minimum_nights ~ price, data=initial_df), col="blue", lwd=3, lty=2)

boxplot(price ~ minimum_nights, data=initial_df, main="Boxplot for Price vs Minimum Nights")

ggplot(initial_df, aes(price, minimum_nights)) +
  geom_boxplot(aes(group = cut_width(price, 0.25))) 

#
# Property Type vs listings
#

categories = as.data.frame(unlist(initial_df$property_type))
colnames(categories) = c("PropertyType")

fillColor = "#FFA07A"
fillColor2 = "#F1C40F"

categories %>%
  group_by(PropertyType) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count)) %>%
  ungroup() %>%
  head(10) %>%
  mutate(PropertyType = reorder(PropertyType,Count)) %>%
  ggplot(aes(x = PropertyType,y = Count)) +
  geom_bar(stat='identity',colour="white", fill =fillColor2) +
  #geom_text(aes(x = PropertyType, y = 1, label = paste0("(",Count,")",sep="")),
  #         colour = 'black') +
  labs(x = 'Property Type', y = 'Count', 
       title = 'Property Type') 


#
# Neighbourhood vs listings
#

categories = as.data.frame(unlist(initial_df$neighbourhood_group_cleansed))
colnames(categories) = c("Neighbourhood")

categories %>%
  group_by(Neighbourhood) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count)) %>%
  ungroup() %>%
  mutate(Neighbourhood = reorder(Neighbourhood,Count)) %>%
  ggplot(aes(x = Neighbourhood,y = Count)) +
  geom_bar(stat='identity',colour="white", fill =fillColor2) +
  #geom_text(aes(x = Neighbourhood, y = 1, label = paste0("(",Count,")",sep="")),
  #         colour = 'black') +
  labs(x = 'Neighbourhood', y = 'Count', 
       title = 'Neighbourhood vs listings') 


get_amenities <- function (df){
  amenities <- stringr::str_sub(df$amenities, 2, -2)
  categories <- as.data.frame(unlist(categories))
  amenities <- gsub('"', '',amenities)
  categories <- stringr::str_split(amenities,",")
  categories <- as.data.frame(unlist(categories))
  colnames(categories) <- c("Name")
  return(categories)
}



# Top 30 Amenities

categories <- get_amenities(initial_df)

categories %>%
  group_by(Name) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count)) %>%
  ungroup() %>%
  head(10) %>%
  mutate(Name = reorder(Name,Count)) -> top10Amenities

# View(top10Amenities)
top10Amenities

ggplot(top10Amenities, aes(x = Name,y = Count)) +
  geom_bar(stat='identity',colour="white", fill =fillColor2) +
  geom_text(aes(x = Name, y = 1, label = paste0("",sep="")),
            hjust=0, vjust=.5, size = 4, colour = 'black',
            fontface = 'bold') +
  labs(x = 'Name', y = 'Count', 
       title = 'Top 10 Amenities') +
  coord_flip() + 
  theme_bw()

# top 10 amenities by neighbourhood

df2 <- filter(.data = initial_df, neighbourhood_group_cleansed == 'Bronx')

categories <- get_amenities(df2)

categories %>%
  group_by(Name) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count)) %>%
  ungroup() %>%
  mutate(Name = reorder(Name,Count)) %>%
  head(10) -> top10AmenitiesBronx

plot2 <- ggplot(top10AmenitiesBronx, aes(x = Name,y = Count)) +
  geom_bar(stat='identity',colour="white", fill =fillColor2) +
  geom_text(aes(x = Name, y = 1, label = paste0("(",Count,")",sep="")),
            hjust=0, vjust=.5, size = 4, colour = 'black',
            fontface = 'bold') +
  labs(x = 'Name', y = 'Count', 
       title = 'Bronx Top 10 Amenities') +
  coord_flip() + 
  theme_bw()


df2 <- filter(.data = initial_df, neighbourhood_group_cleansed == 'Brooklyn')

categories <- get_amenities(df2)

categories %>%
  group_by(Name) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count)) %>%
  ungroup() %>%
  mutate(Name = reorder(Name,Count)) %>%
  head(10) -> top10AmenitiesBrooklyn

plot3 <- ggplot(top10AmenitiesBrooklyn, aes(x = Name,y = Count)) +
  geom_bar(stat='identity',colour="white", fill =fillColor2) +
  geom_text(aes(x = Name, y = 1, label = paste0("(",Count,")",sep="")),
            hjust=0, vjust=.5, size = 4, colour = 'black',
            fontface = 'bold') +
  labs(x = 'Name', y = 'Count', 
       title = 'Brooklyn Top 10 Amenities') +
  coord_flip() + 
  theme_bw()

plot3

df2 <- filter(.data = initial_df, neighbourhood_group_cleansed == 'Queens')

categories <- get_amenities(df2)

categories %>%
  group_by(Name) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count)) %>%
  ungroup() %>%
  mutate(Name = reorder(Name,Count)) %>%
  head(10) -> top10AmenitiesQueens

plot4 <- ggplot(top10AmenitiesQueens, aes(x = Name,y = Count)) +
  geom_bar(stat='identity',colour="white", fill =fillColor2) +
  geom_text(aes(x = Name, y = 1, label = paste0("(",Count,")",sep="")),
            hjust=0, vjust=.5, size = 4, colour = 'black',
            fontface = 'bold') +
  labs(x = 'Name', y = 'Count', 
       title = 'Queens Top 10 Amenities') +
  coord_flip() + 
  theme_bw()


df2 <- filter(.data = initial_df, neighbourhood_group_cleansed == 'Staten Island')

categories <- get_amenities(initial_df)

categories %>%
  group_by(Name) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count)) %>%
  ungroup() %>%
  mutate(Name = reorder(Name,Count)) %>%
  head(10) -> top10AmenitiesStatenIsland

plot5 <- ggplot(top10AmenitiesStatenIsland, aes(x = Name,y = Count)) +
  geom_bar(stat='identity',colour="white", fill =fillColor2) +
  geom_text(aes(x = Name, y = 1, label = paste0("(",Count,")",sep="")),
            hjust=0, vjust=.5, size = 4, colour = 'black',
            fontface = 'bold') +
  labs(x = 'Name', y = 'Count', 
       title = 'Staten Island Top 10 Amenities') +
  coord_flip() + 
  theme_bw()

plot5


# superhost vs Non super host amenities

df2 <- filter(.data = initial_df, host_is_superhost == 1)

categories <- get_amenities(df2)

categories %>%
  group_by(Name) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count)) %>%
  ungroup() %>%
  mutate(Name = reorder(Name,Count)) -> superHostAmenities

df2 <- initial_df %>%
  filter(host_is_superhost == 0 && bedrooms == 1)

categories %>%
  group_by(Name) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count)) %>%
  ungroup() %>%
  mutate(Name = reorder(Name,Count)) -> nonSuperHostAmenities

setdiff(superHostAmenities, nonSuperHostAmenities)


# EDA - Airbnb Reviews Dataset

reviews_data <- read.csv("./data/reviews-summary.csv", nrows = 100000)

docs <- Corpus(VectorSource(reviews_data$comments))

toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")
docs <- tm_map(docs, content_transformer(tolower))
# Remove numbers
docs <- tm_map(docs, removeNumbers)
# Remove english common stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))
# Remove punctuations
docs <- tm_map(docs, removePunctuation)
# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)

dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)

set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

# find word associations

findAssocs(dtm, terms = "like", corlimit = 0.1)
findAssocs(dtm, terms = "good", corlimit = 0.1)
findAssocs(dtm, terms = "near", corlimit = 0.1)
findAssocs(dtm, terms = "great", corlimit = 0.1)
findAssocs(dtm, terms = "close", corlimit = 0.1)
asc <- findAssocs(dtm, terms = "around", corlimit = 0.1)
findAssocs(dtm, terms = "place", corlimit = 0.1)
findAssocs(dtm, terms = "transit", corlimit = 0.1)
findAssocs(dtm, terms = "store", corlimit = 0.1)

# some interesting findings here, found amenities like grocery, park, subway etc,.


#
# 4. Use foursquare_api.R to get information about near by amenities like parks, subways, restaurants etc,.
#

write.csv(initial_df, "./data/cleaned_listings_data.csv")

# run Foursquare_API.r on cleaned_listings_data.csv file

# final dataset below
final_data <- read.csv("./data/final_data.csv")


#
# 5. Feature Engineering
#


# creating dummies for categorical variables


#
# Feature engineering - Do Hot one encoding for amenities
#


# creating dummies for amenities

final_data$amenities <- stringr::str_sub(final_data$amenities, 2, -2)
final_data$amenities <- gsub('"', '',final_data$amenities)
amenities_list <- stringr::str_split(tolower(final_data$amenities)
                                     ,",")
final_data$amenities <- gsub("[^[:alnum:]]", "_", final_data$amenities)
amenities_list <-  sapply(amenities_list, function(d){
  return(gsub("[^[:alnum:]]", "_", d))
})
lev <- unique(unlist(amenities_list)) # list of all unique amenities
dummies <- do.call(rbind, lapply(amenities_list, function(x) table(factor(x, levels=lev))))
str(dummies)

# creating a calculated variable , amenity count
dummies$amenity_count <- base::apply(dummies, MARGIN = 1, sum)

final_data <- cbind(final_data, dummies)

# creating dummies for other categorical variables

final_data <- dummy_cols(final_data, select_columns = c("cancellation_policy", "property_type", "neighbourhood_cleansed", "bed_type", "neighbourhood_group_cleansed"))

final_data$neighbourhood_cleansed <- NULL;
final_data$neighbourhood_group_cleansed <- NULL;
final_data$is_location_exact <- NULL;
final_data$property_type <- NULL;
final_data$room_type <- NULL;
final_data$bed_type <- NULL;
final_data$cancellation_policy <- NULL;
final_data$transit <- NULL;
final_data$price_per_person <- NULL;
final_data$amenities <- NULL;

# saving the final dataset for modeling
write.csv(final_data, './data/final_dataset.csv')