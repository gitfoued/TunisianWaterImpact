# Sélectionner les colonnes pertinentes
barrage_corr <- barrage[, c("Date", "apports")]
sonned_corr <- Sonned[, c("id", "2023 (en dt)", "2024 (en dt)")]

# Filtrez les données pour chaque année
barrage_2023 <- barrage_corr %>%
  filter(Date >= "2023-01-01" & Date <= "2023-12-31")
barrage_2024 <- barrage_corr %>%
  filter(Date >= "2024-01-01" & Date <= "2024-12-31")
sonned_2023 <- sonned_corr %>%
  filter(!is.na(`2023 (en dt)`))  # Assurez-vous que les données ne sont pas manquantes
sonned_2024 <- sonned_corr %>%
  filter(!is.na(`2024 (en dt)`))  # Assurez-vous que les données ne sont pas manquantes

# Assurez-vous que les données ont la même longueur
min_length <- min(length(barrage_2023$apports), length(sonned_2023$`2023 (en dt)`))
barrage_2023 <- barrage_2023[1:min_length, ]
sonned_2023 <- sonned_2023[1:min_length, ]

min_length <- min(length(barrage_2024$apports), length(sonned_2024$`2024 (en dt)`))
barrage_2024 <- barrage_2024[1:min_length, ]
sonned_2024 <- sonned_2024[1:min_length, ]

# Calculer la corrélation entre la somme du stock des barrages et l'augmentation des tarifs pour chaque année
correlation_2023 <- cor(barrage_2023$apports, sonned_2023$`2023 (en dt)`)
correlation_2024 <- cor(barrage_2024$apports, sonned_2024$`2024 (en dt)`)

# Afficher les résultats
print(paste("Corrélation pour 2023 :", correlation_2023))
print(paste("Corrélation pour 2024 :", correlation_2024))
