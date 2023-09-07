
#INIATIATING----

#Clear memory
rm(list = ls())

# Set working directory
setwd(dir = "Inventaire forestier/TP/Data")

#Load libraries
library(tidyverse)
library(BIOMASS)
library(roxygen2)
library(hrbrthemes)
library(gridExtra)
library(openxlsx)
library(stats)

#import data
data <-readxl::read_xlsx("TP/Data/data.xlsx")

#CALCUL VARIABLES----

data$espece <- paste(data$Genre, data$EPITHETE, sep=" ")

# Fonction pour calculer la hauteur en fonction du diamètre et de la catégorie d'altitude
GetHeight <- function(diam, alt){
 
   # Déterminez la catégorie d'altitude
  categorie <- ifelse(alt >= 1250 & alt < 1500, 1,
                      ifelse(alt >= 1500 & alt < 1800, 2,
                             ifelse(alt >= 1800 & alt < 2400, 3,
                                    ifelse(alt >= 2400 & alt <= 2600, 4, NA))))
  
  # Appliquez la formule en fonction de la catégorie
  height <- ifelse(!is.na(categorie), 
                    ifelse(categorie == 1, 30.61*exp(-2.7*exp(-0.95*diam)),
                           ifelse(categorie == 2, 30*exp(3.2*exp(-0.94*diam)),
                                  ifelse(categorie == 3, 22.7-24.41*exp(-exp(-3.3)*diam),
                                         ifelse(categorie == 4, -15.26+11.57*log(diam)-1.17*((log(diam)^2)), NA)))),
                    NA)
  
  return(height)
}

#Ajouter une colonne calculée des hauteurs
data <- data |> 
  mutate(Height = GetHeight(diam =data$`DIAMETRE EN CM`,
                            alt = data$`Altitude en m`))

#Get volume
getvolume <- function(circonference, height){
  #Cette formule permet de calculer le volume par arbre
  
  volume = (((circonference^2)/100)*height)/4*3.14
}

#Ajouter une colonne calculée de volume

data <- data |> 
  mutate(volume_en_m3 = getvolume(circonference = `CIRCONFERENCE EN CM`,
                                  height = Height))

#Get Basal area 
getBasalArea <- function(diam){
# Cette fonction calcule la surface terrière en m²/ha

# diam : Colonne des diametres
  BasalArea = (3.14*(diam*0.01)^2)/4
}

#Créer une colonne calculée de surface terriere
data <- data |> 
  mutate(BasalArea = getBasalArea(diam = `DIAMETRE EN CM`))


#Create a function for get wood density

getWooDen <- function(genus, species, region = "World" ){
  #data : la data frame
  #genre : La colonne avec le genre
  #species : La colonne avec l'epithète
  #region : La region ciblée
  
  #Load Wood density
  density <- getWoodDensity(
    genus = genus,
    species = species,
    stand = NULL,
    family = NULL,
    region = region,
    addWoodDensityData = NULL,
    verbose = TRUE
  )
  
  return(meanWD = density$meanWD)
}


#Create a new  "density" column with mean density value
data <- data |> 
  mutate(WoodDensity = getWooDen(genus = Genre,
                                 species = EPITHETE))


#Get "biomass" per tree

GetBiomass <- function(diam, height, density){
  #diam : Diameter's column in cm
  #height : Height's column in m

  biomass_en_kg = density * exp(-2.977 + log(density * (diam*1)^2 * height))
  
  return(biomass_en_kg)
}


#Créer une colonne calculée de la biomass par arbre
data <- data %>%
  mutate(biomass_en_kg = GetBiomass(diam = `DIAMETRE EN CM`,
                                    height = Height,
                                    density = WoodDensity))

#Diagramme de tranches----


# Création d'un nouveau dataframe avec les groupes de méthodes
data <- data |> 
  mutate(groupe = case_when(
    METHODES %in% c("T1", "T2", "T4", "T5", "T6") ~ "Transect",
    METHODES %in% c("P1", "P2") ~ "Placeau",
    TRUE ~ "Autres"
  ))

# Création du diagramme en tranches de diamètres pour chaque groupe de méthodes
# ggplot(data_groupes, aes(x = `DIAMETRE EN CM`, fill = groupe)) +
#   geom_histogram(binwidth = 5) +
#   labs(title = "Histogrammes des diamètres par groupe de méthodes", x = "Diamètre", y = "Fréquence") +
#   scale_fill_manual(values = c("Transect 1" = "red", "Transect 2" = "blue", "Placeau 1" = "green", "Placeau 2" = "purple", "Autres" = "gray")) +
#   theme_minimal()

# data_groupe1 <- data_groupes %>%
#   filter(groupe == "Transect 1")

# # Création de l'histogramme des diamètres pour le groupe 1
# ggplot(data_groupe1, aes(y = `DIAMETRE EN CM`, fill = groupe)) +
#   geom_histogram(binwidth = 5, position = "dodge") +
#   labs(title = "Histogramme des diamètres pour le groupe 1", x = "Diamètre", y = "Fréquence") +
#   scale_fill_manual(values = c("Transect 1" = "red")) +
#   theme_minimal()


# Créer des intervalles de diamètres
intervalles <- cut(data$`DIAMETRE EN CM`, breaks = c(0, 10, 20, 30, 40, 50), right = FALSE)
# 

# 
donnees_intervalles <- data.frame(Intervalles = intervalles)

# Compter le nombre d'arbres dans chaque intervalle
nombre_arbres_par_intervalle <- table(donnees_intervalles$Intervalles)
# 
# # Créer un dataframe avec les comptages
 df_comptage <- data.frame(Intervalles = names(nombre_arbres_par_intervalle),
                           Nombre_Arbres = as.numeric(nombre_arbres_par_intervalle))


# Fonction pour créer les graphiques pour un groupe donné
create_group_plots <- function(data, groupe) {
  # Filtrer les données pour le groupe spécifié
  data_group <- data %>% filter(groupe == groupe)
  
  # Créer un graphique faceté pour chaque METHODES
  gg <- ggplot(data_group, aes(x = intervalles)) +
    geom_bar(stat = "count", position = "dodge") +
    labs(title = paste("Nombre d'arbres par intervalle de diamètre -", groupe),
         x = "Intervalle de diamètre",
         y = "Nombre d'arbres") +
    theme_minimal() +
    facet_wrap(~METHODES, ncol = 2)  # Vous pouvez ajuster le nombre de colonnes ici
  
  return(gg)
}

transect <- create_group_plots(data = data,
                                 groupe = "Transect")
# 
# 

#DIAGRAMME PAR TRANCHES DE DIAMETRE----

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
    labs(y = "Nombres d'arbres", x = "Diamètre (en cm)",
         title=titre,
         subtitle= sous_titre,
         caption= sourcE)+
    theme(legend.position = "none", axis.text.x = element_text(size = 8)) -> gg
  
  return(gg)
}

#Diagramme Transect
transect_graph <- create_histogram_faceted(data = data,
                                             groupe = "Transect",
                                             facet.labs = c(
                                               "T1" = "Premier 100m",
                                               "T2" = "Deuxième 100m",
                                               "T4" = "Quatrième 100m",
                                               "T5" = "Cinquième 100m",
                                               "T6" = "Sixième 100m"),
                                             titre = "Diagramme de tranches de diamètre",
                                             sous_titre = "Transect",
                                             sourcE = "Source: Données d'inventaire",
                                           ncol = 3)

transect_graph
ggsave(filename = "output/transect.png",
       plot = transect_graph,
       width = 10,
       height = 6,
       bg = "white")


#Diagramme Placeau
placeau_graph <- create_histogram_faceted(data = data,
                                          groupe = "Placeau",
                                          ncol = 2,
                                          facet.labs = c("P2" = "Placeau 2",
                                                            "P1" = "Placeau 1"),
                                          titre = "Diagramme de tranches de diamètre",
                                          sous_titre = "Placeaux",
                                          sourcE = "Source: Données d'inventaire")

placeau_graph
ggsave(filename = "output/placeau.png",
       plot = placeau_graph,
       width = 10,
       height = 6,
       bg = "white")

tout_graph <-ggplot(data, aes(x = cut(`DIAMETRE EN CM`, breaks = c(10, 20, 30, 40, 50, 80, 100, 300), labels = c("11-20", "21-30", "31-40", "41-50", "51-80", "81-100", "101-300")), fill = groupe)) +
  geom_bar(position = "dodge") +
  facet_wrap(~ groupe, ncol = 2) +
  scale_color_ipsum( ) +
  scale_fill_ipsum( ) +
  theme_ipsum_rc() +
  labs(y = "Nombres d'arbres", x = "Diamètre (en cm)",
       title="Diagramme de tranches",
       subtitle= "En fonction de la méthode",
       caption= "Source : Données d'inventaire") +
  theme(legend.position = "none", axis.text.x = element_text(size = 8))

tout_graph
ggsave(filename = "output/tout_graph.png",
       plot = tout_graph,
       width = 10,
       height = 6,
       bg = "white")
#rm NA

data <- data |> 
  na.omit()

#ANNOVA----

# # Créez un dataframe agrégé pour le nombre d'espèces et le nombre d'individus par groupe et méthode
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

#NOMBRES D'ESPECES----
#


# Ensuite, vous pouvez obtenir la liste des espèces uniques en utilisant la fonction unique
liste_especes <- unique(data$espece)
liste_genres <- unique(data$Genre)
# Afficher la liste des espèces
print(liste_especes)

#NBRE DE GENRE----
# data |> 
#   liste_genres <- unique(Genre) |> 
#   nbre_genres <- 
#   n(liste_genres)


#TABLEAU----
result <- data %>%
  group_by(espece, METHODES) %>%
  summarise(NombreOccurrences = n()) %>%
  ungroup()

# Utilisez pivot_wider pour obtenir un tableau à deux entrées
result_wide <- pivot_wider(result, names_from = METHODES, values_from = NombreOccurrences, values_fill = 0)

# Remplacez les valeurs NA par 0
result_wide[is.na(result_wide)] <- 0

# Affichez le résultat
print(result_wide)


#ANALYSE 1----

data <- data %>%
  mutate(Superficie_ha = ifelse(groupe == "Transect", 0.19, 2))

#Calcul nombre d'especes et nbre de genres
data_methodes1 <- data %>%
  group_by(METHODES, groupe) %>%
  reframe(Nombre_Especes = n_distinct(EPITHETE),
            Nombre_Genres = n_distinct(Genre))
data_methodes <- data %>%
  group_by(METHODES, groupe) %>%
  reframe(Nombre_Especes = n_distinct(EPITHETE),
          Nombre_Genres = n_distinct(Genre),
          Superficie_ha = Superficie_ha)
#Calcul densité genre par ha and espece par ha
data_variables <- data_methodes %>%
  group_by(groupe, METHODES) %>%
  reframe(Densite_Moyenne_Genre_ha = sum(Nombre_Genres) / sum(Superficie_ha),
            Densite_Espece_ha = sum(Nombre_Especes) / sum(Superficie_ha))

#Calcul 
data_variables1 <- data %>%
  group_by(groupe, METHODES) %>%
  reframe(Volume_m3_ha = sum(volume_en_m3) / sum(Superficie_ha),
            BasalArea_ha = sum(BasalArea) / sum(Superficie_ha),
          biomass_ha = sum(biomass_en_kg) / sum(Superficie_ha))

liste_especes <- as.data.frame(liste_especes)
liste_genres <- as.data.frame(liste_genres)


#define sheet names for each data frame
dataset_names <- list('Sheet1' = data, 'Sheet2' = data_methodes1, 'Sheet3' = data_variables,
                      'Sheet4' = data_variables1, 'sheet5' = liste_especes, 'Sheet6' = liste_genres)

#export each data frame to separate sheets in same Excel file
openxlsx::write.xlsx(dataset_names, file = "D:/G3 GSEA/COURS/Inventaire forestier/TP/Data/myData3.xlsx") 

