





# Estimation du carbone stocké
biomasse_totale <- sum(data$volume_en_m3, na.rm = TRUE)


#
# Calcul du carbone stocké
carbone_stocke <- biomasse_totale * 0.5


# Aggrégation de la biomasse par espèce
biomasse_par_espece <- aggregate(biomass_en_kg ~ genre, data = data, FUN = sum)

# Visualisation de la biomasse par espèce

biomass_per_species <- ggplot(biomasse_par_espece, aes(x = reorder(genre, -biomass_en_kg), y = biomass_en_kg)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Biomasse par espèce", x = "Espèce", y = "Biomasse (kg)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(filename = "imgs/biomass_per_species.png",
       plot = biomass_per_species,
       width = 10,
       height = 6,
       bg = "white")

