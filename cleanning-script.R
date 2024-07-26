# Charger la bibliothèque tidyverse pour la manipulation des données
library(tidyverse)
library(ggplot2)
# Créer le dataframe avec les données fournies
Sonned_prediction_data <-Sonned
# Renommer les colonnes pour la facilité d'utilisation
colnames(Sonned_prediction_data)[3:4] <- c("prix_2023", "prix_2024")

# Ajuster un modèle de régression linéaire pour prédire les prix d'eau par tranche pour les années 2025 à 2034
model <- lm(prix_2024 ~ prix_2023, data = Sonned_prediction_data)

# Prédire les prix d'eau par tranche pour les années 2025 à 2034
predictions <- predict(model, newdata = data.frame(prix_2023 = Sonned_prediction_data$prix_2024))

# Ajouter les prédictions au dataframe
Sonned_prediction_data$`2025` <- predictions
# Calculer la différence entre les prix prédits pour 2025et les prix de 2024
difference <- Sonned_prediction_data$`2025` - Sonned_prediction_data$prix_2024
difference <- Sonned_prediction_data$`2025` - Sonned_prediction_data$prix_2024


# Créer un nouveau dataframe contenant la tranche et la différence calculée
difference_data <- data.frame(tranche = Sonned_prediction_data$`tranche-metre-cube-par-trimestre`, difference = difference)
#Convertir la variable tranche en un facteur avec un ordre spécifique
difference_data$tranche <- factor(difference_data$tranche, levels = c("0-20", "20-40", "40-70", "70-100", "100-150", "150 et plus", "usage touristique"))

# Tracer la courbe avec la variable tranche dans l'ordre spécifié
ggplot(difference_data, aes(x = tranche, y = difference)) +
  geom_line(color = "green") +
  geom_point(color = "red") +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +  # Ajouter une droite de régression linéaire
  labs(x = "Tranche", y = "Différence de prix", title = "préduction d'augmentation des prix d'eau par tranche en 2025") +
  theme_minimal()



