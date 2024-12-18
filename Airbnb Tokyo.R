#Upload packages and data
library(ggplot2)
library(dplyr)

df <- read.csv("C:/Users/scheu/Downloads/listings.csv")
head(df)

#Clean the database
df$price <- as.numeric(gsub("[$,]", "", df$price)) #remove the $ symbol in the price column
df <- df[df$price != "", ] #remove airbnb with no price repertoried
df <- df[df$price < 100000, ] #remove airbnb with price greater than 100000
df <- df[rowSums(is.na(df)) < ncol(df), ] 

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

db <- cbind(
  df[, c("id", "neighbourhood_cleansed", "price")],                      # Correctly reference the columns by name
  df[, tail(names(df), 13)],                                             # Add the last 12 distance columns
  df[, c("room_type", "bedrooms", "review_scores_rating", 
         "minimum_nights", "availability_365")]                           # Correctly reference the additional columns
)


#-----------------------------------------------------
#------------------ DESCRIPTIVES ---------------------
summary(db)

library(stargazer)

names(db)

subset_db <- db[, c("price", "distance_to_Senso-ji_Temple", "distance_to_Meiji_Jingu_Shrine",
                    "distance_to_teamLab_Planets", "distance_to_Tokyo_Skytree", "distance_to_Tokyo_Tower",
                    "distance_to_Ueno_Park", "distance_to_Ginza_Station", "distance_to_Akihabara_Station",
                    "distance_to_Shibuya_Crossing", "distance_to_Tokyo_National_Museum", 
                    "distance_to_Tokyo_Metropolitan_Government_Buildings")]

stargazer(subset_db, 
          type = "text", 
          title = "Table: Descriptive statistics of some variables")

# Créer un modèle de régression linéaire entre price et les 10 dernières colonnes
log_ols <- lm(log(price) ~ ., data = db[, c(3:15)])
summary(log_ols)

#Tester la multicolinéarité
alias(log_ols)
# Tokyo National Museum parfaitement multicollinéaire

options(scipen=999)
# Résumé du modèle de régression
summary(regression_model)


###Shinjuku Ku### 
#Tokyo Metropolitan Government Buildings / Shinjuku Golden Gai /
  
db1 <- filter(db, neighbourhood_cleansed == "Shinjuku Ku")
m1 <- lm(log(price)~distance_to_Shinjuku_Golden_Gai+distance_to_Tokyo_Metropolitan_Government_Buildings+bedrooms, data = db1)

###Taito Ku###
#Senso-ji Temple / Ueno Park / Tokyo National Museum

db2 <- filter(db, neighbourhood_cleansed == "Taito Ku")
m2 <- lm(log(db2$price)~db2$`distance_to_Senso-ji_Temple`+db2$distance_to_Ueno_Park+db2$distance_to_Tokyo_National_Museum+db2$bedrooms)

###Shibuya Ku###
#Meiji Jingu Shrine / Shibuya Crossing

db3 <- filter(db, neighbourhood_cleansed == "Shibuya Ku")
m3 <- lm(log(db3$price)~db3$distance_to_Meiji_Jingu_Shrine+db3$bedrooms)

###Koto Ku###
#teamLab Planets

db4 <- filter(db, neighbourhood_cleansed == "Koto Ku")
m4 <- lm(log(db4$price)~db4$distance_to_teamLab_Planets+db4$bedrooms)

###Sumida Ku###
#Tokyo Skytree

db5 <- filter(db, neighbourhood_cleansed == "Sumida Ku")
m5 <- lm(log(db5$price)~db5$distance_to_Tokyo_Skytree+db5$bedrooms)


###Minato Ku###
#Tokyo Tower

db6 <- filter(db, neighbourhood_cleansed == "Minato Ku")
m6 <- lm(log(db6$price)~db6$distance_to_Tokyo_Tower+db6$bedrooms)

###Chuo Ku###
#Ginza Station

db7 <- filter(db, neighbourhood_cleansed == "Chuo Ku")
m7 <- lm(log(db7$price)~db7$distance_to_Ginza_Station+db7$bedrooms)

###Chiyoda Ku###
#Akihabara Station 

db8 <- filter(db, neighbourhood_cleansed == "Chiyoda Ku")
m8 <- lm(log(db8$price)~db8$distance_to_Akihabara_Station+db8$bedrooms)

