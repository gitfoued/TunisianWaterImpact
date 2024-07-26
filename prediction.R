# Charger la librairie ggplot2
library(ggplot2)

# Division des données en ensemble d'entraînement et ensemble de test
set.seed(123)
train_index <- sample(1:nrow(barrage), 0.8 * nrow(barrage))
train_data <- barrage[train_index, ]
test_data <- barrage[-train_index, ]

# Ajustement du modèle de régression linéaire
model <- lm(stock ~ apports, data = train_data)

# Prédiction sur l'ensemble de test
predictions <- predict(model, newdata = test_data)

# Évaluation du modèle
R_squared <- summary(model)$r.squared
RMSE <- sqrt(mean((test_data$stock - predictions)^2))
mean_apports <- mean(barrage$apport)
mean_Stock <- mean(barrage$stock)
print(mean_Stock)
# Prédiction pour l'état des barrages après 10 ans
# Supposons des valeurs fictives pour les apports pour les 10 prochaines années
apports_future <- rep(apports_future, 10)  # Remplacez "valeur_apports_moyenne" par la valeur moyenne des apports
new_years <- 2025:2034

# Création du nouveau dataframe avec les valeurs fictives
new_data <- data.frame(apports = apports_future, année = new_years)

# Prédiction pour l'état des barrages après 10 ans
predictions_10_years <- predict(model, newdata = new_data)

# Création d'un dataframe pour les prédictions à 10 ans
predictions_df_10_years <- data.frame(Année = new_years, Prédictions = predictions_10_years)

# Affichage des résultats
print(paste("R-squared:", R_squared))
print(paste("RMSE:", RMSE))
print(paste("Prédiction pour l'état des barrages après 10 ans:", predictions_10_years))

# Visualisation des prédictions
ggplot(predictions_df_10_years, aes(x = Année, y = Prédictions)) +
  geom_line(color = "blue") +
  geom_point(color = "blue", size = 3) +
  labs(x = "Année", y = "Prédictions du modèle pour le stock des barrages", title = "Prédictions du modèle pour le stock des barrages au fil des années") +
  theme_minimal()
