#Voici la description des variables à notre disposition :
  
#df : Base de données originale 
#db : Base de donnée nettoyée pour l’analyse


#-- Upload packages and data
rm(list = ls())

library(stringr)
library(lmtest)
library(sandwich)
library(stargazer)
library(corrplot)
library(dplyr)
options(scipen=999)


#database available at : https://insideairbnb.com/get-the-data/
df <- read.csv("listings.csv")
head(df)

#Clean the database
df$price <- as.numeric(gsub("[$,]", "", df$price)) #remove the $ symbol in the price column
df <- df[df$price != "", ] #remove airbnb with no price repertoried
df <- df[df$price < 100000, ] #remove airbnb with price greater than 100000
df <- df[rowSums(is.na(df)) < ncol(df), ] #remove rows with missing values

# Create at dataframe for every important touristic spot in Tokyo
name <- c("Shinjuku Golden Gai", "Senso-ji Temple", "Meiji Jingu Shrine", "teamLab Planets",
          "Tokyo Skytree", "Tokyo Tower", "Ueno Park", "Ginza Station", "Akihabara Station",
          "Shibuya Crossing", "Tokyo National Museum", "Tokyo Metropolitan Government Buildings") #vector for the names
latitude <- c(35.4138, 35.4252, 35.4034, 35.6485, 35.4236, 35.3931, 35.4244, 35.4016,
              35.4154, 35.3934, 35.4308, 35.4123) #vector for the latitude
longitude <- c(139.4217, 139.4748, 139.4157, 139.7899, 139.4839, 139.4444, 139.4616,
               139.4554, 139.4623, 139.4202, 139.4633, 139.4132) #vector for the longitude

tourist <- data.frame(name, latitude, longitude) #combined vectors

#Distance calculation function (Haversine formula)
haversine_distance <- function(lat1, lon1, lat2, lon2, R = 6371) {
  to_radians <- function(degrees) degrees * pi / 180
  lat1 <- to_radians(lat1)
  lon1 <- to_radians(lon1)
  lat2 <- to_radians(lat2)
  lon2 <- to_radians(lon2)
  
  delta_lat <- lat2 - lat1
  delta_lon <- lon2 - lon1
  
  a <- sin(delta_lat / 2)^2 + cos(lat1) * cos(lat2) * sin(delta_lon / 2)^2
  c <- 2 * asin(sqrt(a))
  d <- R * c  # Distance en kilomètres
  return(d)
}

# Create at dataframe for every important touristic spot in Tokyo
name <- c("Shinjuku Golden Gai", "Senso-ji Temple", "Meiji Jingu Shrine", "teamLab Planets",
          "Tokyo Skytree", "Tokyo Tower", "Ueno Park", "Ginza Station", "Akihabara Station",
          "Shibuya Crossing", "Tokyo National Museum", "Tokyo Metropolitan Government Buildings") #vector for the names
latitude <- c(35.4138, 35.4252, 35.4034, 35.6485, 35.4236, 35.3931, 35.4244, 35.4016,
              35.4154, 35.3934, 35.4308, 35.4123) #vector for the latitude
longitude <- c(139.4217, 139.4748, 139.4157, 139.7899, 139.4839, 139.4444, 139.4616,
               139.4554, 139.4623, 139.4202, 139.4633, 139.4132) #vector for the longitude

tourist <- data.frame(name, latitude, longitude) #combined vectors

# Calculer les distances pour chaque appartement et chaque site touristique
for (i in 1:nrow(df)) {
  for (j in 1:nrow(tourist)) {
    col_name <- paste0("distance_to_", gsub(" ", "_", tourist$name[j]))  # Créer un nom de colonne dynamique
    # Calculer la distance pour cet appartement et ce site
    df[i, col_name] <- haversine_distance(
      lat1 = df$latitude[i],
      lon1 = df$longitude[i],
      lat2 = tourist$latitude[j],
      lon2 = tourist$longitude[j]
    )
  }
}


# Binary variables to check the presence of some equipments/characteristics
df <- mutate(df,TV = ifelse(str_detect(amenities, "TV"), 1, 0))
df <- mutate(df,View = ifelse(str_detect(amenities, "view"), 1, 0))
df <- mutate(df,Self_check_in = ifelse(str_detect(amenities, "Self check-in"), 1, 0))

# Cleaned database
db <- cbind(
  df[, c("id", "neighbourhood_cleansed", "price")],
  df[, tail(names(df), 15)],
  df[, c("bedrooms", "review_scores_rating", "minimum_nights")]
)



# Check for NA in the database
colSums(is.na(db))
db <- na.omit(db)

#-----------------------------------------------------
#------------------ DESCRIPTIVES ---------------------
summary(db)

#Descriptive statistics table
str(db)
db$bedrooms <- as.numeric(db$bedrooms)
db$minimum_nights <- as.numeric(db$minimum_nights)
stargazer(db,
          type = "text",
          title = "Table: Descriptive statistics of some variables")

# Compute the correlation coefficients
corr.matrix <- round(cor(db[,3:21]), 2)

# Simple linear regression
# Simple linear regression
ols <- lm(price ~ ., data = db[,3:21])
log_ols <- lm(log(price) ~ ., data = db[,3:21])
stargazer(ols, log_ols, type = "text", align = TRUE) # Comparison
              
# Test for multicollinearity
alias(log_ols)
dwtest(log_ols)
bptest(log_ols)
white_ols <- coeftest(log_ols, vcov = vcovHC(log_ols, type = "HC1"))

stargazer(log_ols, white_ols, type = "text", align = TRUE) # Comparison

###Shinjuku Ku###
#Tokyo Metropolitan Government Buildings / Shinjuku Golden Gai /

db1 <- filter(db, neighbourhood_cleansed == "Shinjuku Ku")
m1 <- lm(log(price)~distance_to_Shinjuku_Golden_Gai+distance_to_Tokyo_Metropolitan_Government_Buildings+TV+View+Self_check_in+bedrooms+review_scores_rating+minimum_nights, data = db1)
bptest(m1)
coeftest(m1, vcov = vcovHC(m1, type = "HC1"))


###Taito Ku###
#Senso-ji Temple / Ueno Park / Tokyo National Museum

db2 <- filter(db, neighbourhood_cleansed == "Taito Ku")
m2 <- lm(log(db2$price)~db2$`distance_to_Senso-ji_Temple`+distance_to_Ueno_Park+distance_to_Tokyo_National_Museum+TV+View+Self_check_in+bedrooms+review_scores_rating+minimum_nights, data = db2)
bptest(m2)
coeftest(m2, vcov = vcovHC(m2, type = "HC1"))


###Shibuya Ku###
#Meiji Jingu Shrine / Shibuya Crossing

db3 <- filter(db, neighbourhood_cleansed == "Shibuya Ku")
m3 <- lm(log(db3$price)~distance_to_Meiji_Jingu_Shrine+TV+View+Self_check_in+bedrooms+review_scores_rating+minimum_nights, data = db3)
bptest(m3)
coeftest(m3, vcov = vcovHC(m3, type = "HC1"))
###Koto Ku###
#teamLab Planets

db4 <- filter(db, neighbourhood_cleansed == "Koto Ku")
m4 <- lm(log(db4$price)~distance_to_teamLab_Planets+TV+View+Self_check_in+bedrooms+review_scores_rating+minimum_nights, data = db4)
bptest(m4)
coeftest(m4, vcov = vcovHC(m4, type = "HC1"))
###Sumida Ku###
#Tokyo Skytree

db5 <- filter(db, neighbourhood_cleansed == "Sumida Ku")
m5 <- lm(log(db5$price)~distance_to_Tokyo_Skytree+TV+View+Self_check_in+bedrooms+review_scores_rating+minimum_nights, data = db5)
bptest(m5)
coeftest(m5, vcov = vcovHC(m5, type = "HC1"))

###Minato Ku###
#Tokyo Tower

db6 <- filter(db, neighbourhood_cleansed == "Minato Ku")
m6 <- lm(log(db6$price)~distance_to_Tokyo_Tower+TV+View+Self_check_in+bedrooms+review_scores_rating+minimum_nights, data = db6)
bptest(m6)
coeftest(m6, vcov = vcovHC(m6, type = "HC1"))
###Chuo Ku###
#Ginza Station

db7 <- filter(db, neighbourhood_cleansed == "Chuo Ku")
m7 <- lm(log(db7$price)~distance_to_Ginza_Station+TV+View+Self_check_in+bedrooms+review_scores_rating+minimum_nights, data = db7)
bptest(m7)
coeftest(m7, vcov = vcovHC(m7, type = "HC1"))
###Chiyoda Ku###
#Akihabara Station

db8 <- filter(db, neighbourhood_cleansed == "Chiyoda Ku")
m8 <- lm(log(db8$price)~distance_to_Akihabara_Station+TV+View+Self_check_in+bedrooms+review_scores_rating+minimum_nights, data = db8)
bptest(m8)
coeftest(m8, vcov = vcovHC(m8, type = "HC1"))



              


