library(ggplot2)

df <- read.csv("C:/Users/eelai/OneDrive/Documents/listings.csv")
head(df)

df$price <- as.numeric(gsub("[$,]", "", df$price))
filtered_df <- df[df$price != "", ]
filtered_df <- filtered_df[filtered_df$price < 100000, ]
cleaned_data <- filtered_df[rowSums(is.na(filtered_df)) < ncol(filtered_df), ]

# Fonction de calcul de la distance haversine
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

# Créer un dataframe pour les sites touristiques
name <- c("Shinjuku Golden Gai", "Senso-ji Temple", "Meiji Jingu Shrine", "teamLab Planets", 
          "Tokyo Skytree", "Tokyo Tower", "Ueno Park", "Ginza Station", "Akihabara Station", 
          "Shibuya Crossing", "Tokyo National Museum", "Tokyo Metropolitan Government Buildings")
latitude <- c(35.4138, 35.4252, 35.4034, 35.6485, 35.4236, 35.3931, 35.4244, 35.4016, 
              35.4154, 35.3934, 35.4308, 35.4123)
longitude <- c(139.4217, 139.4748, 139.4157, 139.7899, 139.4839, 139.4444, 139.4616, 
               139.4554, 139.4623, 139.4202, 139.4633, 139.4132)

tourist <- data.frame(name, latitude, longitude)

# Créer un dataframe pour les appartements
geo_appartements <- data.frame(
  id = cleaned_data$id,
  latitude = cleaned_data$latitude,
  longitude = cleaned_data$longitude
)

# Calculer les distances pour chaque appartement et chaque site touristique
for (i in 1:nrow(cleaned_data)) {
  for (j in 1:nrow(tourist)) {
    col_name <- paste0("distance_to_", gsub(" ", "_", tourist$name[j]))  # Créer un nom de colonne dynamique
    # Calculer la distance pour cet appartement et ce site
    cleaned_data[i, col_name] <- haversine_distance(
      lat1 = cleaned_data$latitude[i],
      lon1 = cleaned_data$longitude[i],
      lat2 = tourist$latitude[j],
      lon2 = tourist$longitude[j]
    )
  }
}




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

