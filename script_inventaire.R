# INITIATING----

# Effacer la mémoire (supprime toutes les variables existantes)
rm(list = ls())

# Charger les bibliothèques requises

pacman::p_load(tidyverse, BIOMASS, hrbrthemes, gridExtra, openxlsx, stats, 
               wrMisc, vegan)
stringsAsFactors = FALSE

# Définir le répertoire de travail
path <-"D:/G3 GSEA/COURS/Inventaire forestier/TP/Data/data.xlsx"
# setwd(dir = path)

# Importer les données à partir du fichier Excel
data <- readxl::read_xlsx(path)

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

# Définition de la fonction "GetHeight" pour calculer la hauteur en fonction du diamètre et de l'altitude
GetHeight <- function(diam, alt){
   # Déterminer la catégorie d'altitude
   categorie <- ifelse(alt >= 1250 & alt < 1500, 1,
                      ifelse(alt >= 1500 & alt < 1800, 2,
                             ifelse(alt >= 1800 & alt < 2400, 3,
                                    ifelse(alt >= 2400 & alt <= 2600, 4, NA))))
  
  # Appliquer la formule en fonction de la catégorie
  height <- ifelse(!is.na(categorie), 
                    ifelse(categorie == 1, 30.61*exp(-2.7*exp(-0.95*diam)),
                           ifelse(categorie == 2, 30*exp(3.2*exp(-0.94*diam)),
                                  ifelse(categorie == 3, 22.7-24.41*exp(-exp(-3.3)*diam),
                                         ifelse(categorie == 4, -15.26+11.57*log(diam)-1.17*((log(diam)^2)), NA)))),
                    NA)
  
  return(round(height, 2))
}

# Ajouter une colonne "Height" avec les hauteurs calculées
data <- data |> 
  mutate(Height = GetHeight(diam = data$`DIAMETRE EN CM`,
                            alt = data$`Altitude en m`))

#Ajouteer une colonne 'volume' avec les hauteurs calculées
data <- data %>% 
  mutate(
    volume_en_m3 = getvolume(circonference = data$circonference_en_cm,
                       height = data$height)
  )


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

# Fonction pour créer les diagrammes de tranches (histogrammes) facettés
create_histogram_faceted <- function(data, ncol, groupe, facet.labs, ylab, xlab, titre, sous_titre, sourcE) {
  filtered_data <- data[data$groupe == groupe, ]
  
  ggplot(filtered_data, aes(x = cut(`DIAMETRE EN CM`, breaks = c(10, 20, 30, 40, 50, 80, 100, 300), labels = c("11-20", "21-30", "31-40", "41-50", "51-80", "81-100", "101-300")), fill = METHODES)) +
    geom_bar(position = "dodge") +
    labs(title = paste("Histogramme des arbres pour le groupe", groupe),
         x = "Diamètre",
         y = "Nombre d'arbres") +
    facet_wrap(~ METHODES, ncol = ncol) +
    scale_color_ipsum( ) +
    scale_fill_ipsum( ) +
    theme_ipsum_rc() +
    labs(y = ylab, x = xlab,
         title = titre,
         subtitle = sous_titre,
         caption = sourcE) +
    theme(legend.position = "none", axis.text.x = element_text(size = 8)) -> gg
  
  return(gg)
}


# Suppression des lignes avec des valeurs manquantes (NA)
data <- data |> 
  na.omit()
# Calcul des statistiques et agrégation des données

aggregated_data <- data %>%
  group_by(groupe, METHODES) %>%
  reframe(Nb_individus = n(),
          Nb_especes = n_distinct(data$espece))

# Effectuer une ANOVA pour le nombre d'espèces par groupe
anova_especes <- aov(Nb_especes ~ groupe, data = aggregated_data)

# Effectuer une ANOVA pour le nombre d'individus par groupe
anova_individus <- aov(Nb_individus ~ groupe, data = aggregated_data)

# Résumé des ANOVA
summary(anova_especes)
summary(anova_individus)

# Obtenir la liste des espèces uniques
liste_especes <- unique(data$espece)
liste_genres <- unique(data$Genre)

# Exporter la liste des espèces et des genres en tant que dataframes
liste_especes <- as.data.frame(liste_especes)
liste_genres <- as.data.frame(liste_genres)

#Calcul nombre d'especes et nbre de genres
data_methodes1 <- data %>%
  group_by(METHODES, groupe) %>%
  reframe(Nombre_Especes = n_distinct(EPITHETE),
            Nombre_Genres = n_distinct(Genre))

#Calcul de la superficie
data <- data %>%
  mutate(Superficie_ha = ifelse(groupe == "Transect", 0.19, 2))

#
data_methodes <- data %>%
  group_by(METHODES, groupe) %>%
  reframe(Nombre_Especes = n_distinct(EPITHETE),
          Nombre_Genres = n_distinct(Genre),
          Superficie_ha = Superficie_ha)

#Calcul densité genre par ha and espece par ha
density_per_ha <- data_methodes %>%
  group_by(groupe, METHODES) %>%
  reframe(Densite_Moyenne_Genre_ha = sum(Nombre_Genres) / sum(Superficie_ha),
            Densite_Espece_ha = sum(Nombre_Especes) / sum(Superficie_ha))
#Calcul variables mean per ha
meanVariables_ha <- data %>%
  group_by(groupe, METHODES) %>%
  reframe(Volume_m3_ha = sum(volume_en_m3) / sum(Superficie_ha),
            BasalArea_ha = sum(BasalArea) / sum(Superficie_ha),
          biomass_ha = sum(biomass_en_kg) / sum(Superficie_ha))

#Create a new dataframe "data1" with column 'espece' and 'methodes'

data1 <- data %>%
  reframe(sites = METHODES,
            especes = espece)


#Tableau de presence ou d'absence
pre_abs <- Getpresence(data1, especes, sites)


occurence <- GetOccurence(df = data,
                          espece = espece,
                          sites = METHODES
)

# Définir les noms des feuilles pour chaque dataframe
dataset_names <- list('Sheet1' = data, 'Sheet2' = data_methodes1, 'Sheet3' = density_per_ha, 'Sheet4' = meanVariables_ha, 'sheet5' = pre_abs, 'Sheet6' = occurence)

# Exporter chaque dataframe dans un fichier Excel
openxlsx::write.xlsx(dataset_names, file = "D:/G3 GSEA/COURS/Inventaire forestier/TP/Data/myData.xlsx")

