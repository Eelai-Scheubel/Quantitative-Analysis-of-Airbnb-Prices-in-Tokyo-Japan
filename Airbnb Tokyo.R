#Upload packages and data
library(ggplot2)

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

# Create a new dataframe for the regression
db <- cbind(
  df[c("price")],                      # Correctly reference the columns by name
  df[, tail(names(df), 12)],                   # Add the last 12 columns from df
  df[, c("room_type", "bedrooms", "review_scores_rating", 
         "minimum_nights", "availability_365")]  # Correctly reference the additional columns
)



# Créer un modèle de régression linéaire entre price et les 10 dernières colonnes
regression_model <- lm(log(price) ~ ., data = cleaned_data[, c("price", distance_columns)])


ward <-  cleaned_data[cleaned_data$neighbourhood_cleansed == "Sumida Ku", ]

# Sélectionner les 10 dernières colonnes
distance_columns <- tail(names(ward), 10)

ols <- lm(ward$price~ward$distance_to_Tokyo_Skytree)

summary(ols)

options(scipen=999)
# Résumé du modèle de régression
summary(regression_model)

