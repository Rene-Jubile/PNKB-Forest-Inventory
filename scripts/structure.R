








#STRUCTURE DES DIAMETRES----

data <- data %>% 
  janitor::clean_names()

# Calcul des classes de diamètre. Par exemple, chaque classe représente 10 cm de diamètre.
data$DiametreClasse <- cut(data$diametre_en_cm,
                         breaks = seq(from = min(data$diametre_en_cm, na.rm = TRUE),
                        to = max(data$diametre_en_cm, na.rm = TRUE), by = 10),
                        include.lowest = TRUE)

# Aggrégation des données pour compter le nombre d'arbres par classe de diamètre
diametre_distribution <- aggregate(x = list(NombreArbres = data$diametre_en_cm),
                                   by = list(ClasseDiametre = data$DiametreClasse),
                                   FUN = length)

# Graphique de la distribution diamétrique
structure_dimatre <- ggplot(diametre_distribution, aes(x = ClasseDiametre, y = NombreArbres)) +
  geom_bar(stat = "identity", fill = "forestgreen") +
  theme_minimal() +
  labs(title = "Distribution des classes de diamètre", x = "Classe de diamètre (cm)", y = "Nombre d'arbres") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(filename = "imgs/structure_dimatre.png",
       plot = structure_dimatre,
       width = 10,
       height = 6,
       bg = "white")

#HAUTEUR DES ARBRES----

# Définition des classes de hauteur, par exemple par tranches de 5 mètres
data$HauteurClasse <- cut(data$height, breaks = seq(from = min(data$height, na.rm = TRUE),
                                                to = max(data$height, na.rm = TRUE),
                                                by = 5), include.lowest = TRUE)

# Calcul et visualisation similaires à ceux effectués pour le diamètre
hauteur_distribution <- aggregate(x = list(NombreArbres = data$height),
                                  by = list(ClasseHauteur = data$HauteurClasse),
                                  FUN = length)

str_hauteur <- ggplot(hauteur_distribution, aes(x = ClasseHauteur, y = NombreArbres)) +
  geom_bar(stat = "identity", fill = "blue") +
  theme_minimal() +
  labs(title = "Distribution des classes de hauteur",
       x = "Classe de hauteur (m)", y = "Nombre d'arbres") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(filename = "imgs/str_hauteur.png",
       plot = str_hauteur,
       width = 10,
       height = 6,
       bg = "white")
