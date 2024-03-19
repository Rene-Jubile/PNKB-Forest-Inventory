








# Création d'une table de données pour l'analyse de diversité
# On groupe par espèce et on compte le nombre d'individus par espèce
data_species <- data %>%
  group_by(Genre, EPITHETE) %>%
  summarise(Nombre = n()) %>%
  ungroup()


# Conversion en matrice de communauté nécessaire pour vegan
mat_species <- with(data_species, table(Genre, EPITHETE))

# Calcul de l'indice de diversité de Shannon
shannon_diversity <- diversity(mat_species, index = "shannon")

# Calcul de l'indice de diversité de Simpson
simpson_diversity <- diversity(mat_species, index = "simpson")

# Affichage des résultats
print(shannon_diversity)
print(simpson_diversity)


# Diagramme de barres du nombre d'individus par espèce, organisé par ordre décroissant
diversity <- ggplot(data_species, aes(x = reorder(Genre, -Nombre), y = Nombre, fill = EPITHETE)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Nombre d'individus par espèce", x = "Espèce", y = "Nombre d'individus") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(filename = "imgs/diversity.png",
       plot = diversity,
       width = 10,
       height = 6,
       bg = "white")
