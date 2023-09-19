# INITIATING----

# Effacer la mémoire (supprime toutes les variables existantes)
rm(list = ls())

# Charger les bibliothèques requises
libs <- c("tidyverse", "BIOMASS", "hrbrthemes", "gridExtra", "openxlsx", "stats", "wrMisc", "vegan")
invisible(lapply(libs, library, character.only = T))
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


# Calcul du volume de chaque arbre en fonction de la circonférence et de la hauteur
getvolume <- function(circonference, height){
  # Cette formule permet de calculer le volume par arbre
  volume = (((circonference^2)/100)*height)/4*3.14
  
  return(round(volume, 2))
}

# Ajouter une colonne calculée de volume
data <- data |> 
  mutate(volume_en_m3 = getvolume(circonference = `CIRCONFERENCE EN CM`,
                                  height = Height))

# Définition d'une fonction pour calculer la surface terrière en m²/ha en fonction du diamètre
getBasalArea <- function(diam){
  # Cette fonction calcule la surface terrière en m²/ha
  BasalArea = (3.14*(diam*0.01)^2)/4

  return(round(BasalArea, 2))
}

# Créer une colonne calculée de surface terrière
data <- data |> 
  mutate(BasalArea = getBasalArea(diam = `DIAMETRE EN CM`))

# Définition d'une fonction pour obtenir la densité du bois en fonction du genre et de l'épithète de l'arbre
getWooDen <- function(genus, species, region = "World" ){
  # Load Wood density
  density <- getWoodDensity(
    genus = genus,
    species = species,
    stand = NULL,
    family = NULL,
    region = region,
    addWoodDensityData = NULL,
    verbose = TRUE
  )

  meanWD = density$meanWD
  # return(meanWD = density$meanWD)
  return(round(meanWD, 2))
}

# Ajouter une colonne "WoodDensity" avec les densités calculées
data <- data |> 
  mutate(WoodDensity = getWooDen(genus = Genre,
                                 species = EPITHETE))

# Définition d'une fonction pour calculer la biomasse des arbres en fonction du diamètre, de la hauteur et de la densité
GetBiomass <- function(diam, height, density){
  # diam : Diamètre en cm
  # height : Hauteur en m

  biomass_en_kg = density * exp(-2.977 + log(density * (diam*1)^2 * height))
  
  return(round(biomass_en_kg, 2))
}

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

# Diagramme de tranches pour le groupe "Transect"
transect_graph <- create_histogram_faceted(data = data,
                                           groupe = "Transect",
                                           facet.labs = c(
                                             "T1" = "Premier 100m",
                                             "T2" = "Deuxième 100m",
                                             "T4" = "Quatrième 100m",
                                             "T5" = "Cinquième 100m",
                                             "T6" = "Sixième 100m"),
                                           ylab = "Nombres d'arbres",
                                           xlab = "Diamètre (en cm)",
                                           titre = "Diagramme de tranches de diamètre",
                                           sous_titre = "Transect",
                                           sourcE = "Source: Données d'inventaire",
                                           ncol = 3)

# Diagramme de tranches pour le groupe "Placeau"
placeau_graph <- create_histogram_faceted(data = data,
                                          groupe = "Placeau",
                                          ncol = 2,
                                          facet.labs = c("P2" = "Placeau 2",
                                                          "P1" = "Placeau 1"),
                                          ylab = "Nombres d'arbres",
                                          xlab = "Diamètre (en cm)",
                                          titre = "Diagramme de tranches de diamètre",
                                          sous_titre = "Placeaux",
                                          sourcE = "Source: Données d'inventaire")

# Diagramme de tranches pour l'ensemble des données
tout_graph <- ggplot(data, aes(x = cut(`DIAMETRE EN CM`, breaks = c(10, 20, 30, 40, 50, 80, 100, 300), labels = c("11-20", "21-30", "31-40", "41-50", "51-80", "81-100", "101-300")), fill = groupe)) +
  geom_bar(position = "dodge") +
  facet_wrap(~ groupe, ncol = 2) +
  scale_color_ipsum( ) +
  scale_fill_ipsum( ) +
  theme_ipsum_rc() +
  labs(y = "Nombres d'arbres", x = "Diamètre (en cm)",
       title = "Diagramme de tranches",
       subtitle = "En fonction de la méthode",
       caption = "Source : Données d'inventaire") +
  theme(legend.position = "none", axis.text.x = element_text(size = 8))

# Enregistrement des graphiques en tant qu'images PNG
ggsave(filename = "output/transect.png",
       plot = transect_graph,
       width = 10,
       height = 6,
       bg = "white")

ggsave(filename = "output/placeau.png",
       plot = placeau_graph,
       width = 10,
       height = 6,
       bg = "white")

ggsave(filename = "output/tout_graph.png",
       plot = tout_graph,
       width = 10,
       height = 6,
       bg = "white")

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

#Getpresence_absence
Getpresence <- function(data, especes, sites){

#data : dataframme
#species : species' column
#sites : sites' column

# Création du dataframe de présence/absence
  presence_absence <- data %>%
    group_by(especes, sites) %>%
    summarise(presence = 1) %>%
    pivot_wider(names_from = sites, values_from = presence, values_fill = 0) %>%
    ungroup()

  #Réordonner les colonnes par ordre alphabetique
  presence_absence <- presence_absence %>%
    select(especes, everything()) %>%

  return(presence_absence)
}

#Tableau de presence ou d'absence
pre_abs <- Getpresence(data1, especes, sites)

#occurence
GetOccurence <- function(df, espece, sites){
  result <- df %>%
    group_by({{espece}}, {{sites}}) %>%
    summarise(NombreOccurrences = n()) %>%
    ungroup()
  # Utilisez pivot_wider pour obtenir un tableau à deux entrées
  OccuTab <- pivot_wider(result, names_from = {{sites}}, values_from = NombreOccurrences, values_fill = 0)
  # Remplacez les valeurs NA par 0
  OccuTab[is.na(OccuTab)] <- 0

  return(OccuTab)
}

occurence <- GetOccurence(df = data,
                          espece = espece,
                          sites = METHODES
)

# Définir les noms des feuilles pour chaque dataframe
dataset_names <- list('Sheet1' = data, 'Sheet2' = data_methodes1, 'Sheet3' = density_per_ha, 'Sheet4' = meanVariables_ha, 'sheet5' = pre_abs, 'Sheet6' = occurence)

# Exporter chaque dataframe dans un fichier Excel
openxlsx::write.xlsx(dataset_names, file = "D:/G3 GSEA/COURS/Inventaire forestier/TP/Data/myData.xlsx")
