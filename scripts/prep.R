








# INITIATING----

# Effacer la mémoire (supprime toutes les variables existantes)
rm(list = ls())

# Charger les bibliothèques requises

pacman::p_load(tidyverse, BIOMASS, hrbrthemes, gridExtra, openxlsx, stats, 
               wrMisc, vegan)
stringsAsFactors = FALSE


# CALCUL DES VARIABLES----

# Créer une colonne "groupe" en fonction de la méthode d'inventaire
data <- data |> 
  mutate(groupe = case_when(
    METHODES %in% c("T1", "T2", "T3", "T4", "T5", "T6") ~ "Transect",
    METHODES %in% c("P1", "P2") ~ "Placeau",
    TRUE ~ "Autres"
  ))

# Créer une colonne "espece" en combinant les colonnes "Genre" et "EPITHETE"
data$espece <- paste(data$Genre, data$EPITHETE, sep=" ")


# Ajouter une colonne "Height" avec les hauteurs calculées
data <- data |> 
  mutate(Height = GetHeight(diam = data$`DIAMETRE EN CM`,
                            alt = data$`Altitude en m`))
# Créer une colonne calculée de surface terrière
data <- data |> 
  mutate(BasalArea = getBasalArea(diam = `DIAMETRE EN CM`))


# Ajouter une colonne "WoodDensity" avec les densités calculées
data <- data |> 
  mutate(WoodDensity = getWooDen(genus = Genre,
                                 species = EPITHETE))

# Créer une colonne calculée de la biomasse par arbre
data <- data %>%
  mutate(biomass_en_kg = GetBiomass(diam = `DIAMETRE EN CM`,
                                    height = Height,
                                    density = WoodDensity))



# Diagramme de tranches----

# Créer des intervalles de diamètres
intervalles <- cut(data$`DIAMETRE EN CM`, breaks = c(0, 10, 20, 30, 40, 50), right = FALSE)
donnees_intervalles <- data.frame(Intervalles = intervalles)

# Compter le nombre d'arbres dans chaque intervalle
nombre_arbres_par_intervalle <- table(donnees_intervalles$Intervalles)

# Créer un dataframe avec les comptages des arbres par intervalle de diamètres
df_comptage <- data.frame(Intervalles = names(nombre_arbres_par_intervalle),
                          Nombre_Arbres = as.numeric(nombre_arbres_par_intervalle))

